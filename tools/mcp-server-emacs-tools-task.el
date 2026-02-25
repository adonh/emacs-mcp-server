;;; mcp-server-emacs-tools-task.el --- Task/Subagent MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for launching subtasks (subagent sessions).
;; In the Emacs context, this spawns a new MCP session or runs a command.
;; Mirrors the opencode task.ts tool conceptually.
;;
;; Since Emacs doesn't have native subagent infrastructure, this tool:
;; 1. Records the task request with a unique ID
;; 2. Optionally executes a shell command as the "task"
;; 3. Returns a task_id for resuming

;;; Code:

(require 'mcp-server-tools)
(require 'cl-lib)

(defvar mcp-server-emacs-tools--task-registry (make-hash-table :test 'equal)
  "Registry of launched tasks keyed by task ID.")

(defvar mcp-server-emacs-tools--task-counter 0
  "Counter for generating unique task IDs.")

(defun mcp-server-emacs-tools--task-make-id ()
  "Generate a unique task ID."
  (format "task-%d-%d" (cl-incf mcp-server-emacs-tools--task-counter)
          (random 10000)))

(defun mcp-server-emacs-tools--task-handler (args)
  "Handle task tool invocation with ARGS."
  (condition-case err
      (let* ((description (alist-get 'description args))
             (prompt (alist-get 'prompt args))
             (subagent-type (alist-get 'subagent_type args))
             (task-id (or (alist-get 'task_id args)
                           (mcp-server-emacs-tools--task-make-id))))
        (unless description (error "description is required"))
        (unless prompt (error "prompt is required"))
        (unless subagent-type (error "subagent_type is required"))
        ;; Store task in registry
        (puthash task-id
                 (list :description description
                       :prompt prompt
                       :subagent-type subagent-type
                       :status "pending"
                       :created (current-time))
                 mcp-server-emacs-tools--task-registry)
        ;; In the Emacs implementation, we log the task and return an ID.
        ;; Actual execution would depend on integrating with a subagent system.
        ;; For now, if the prompt looks like a shell command or starts with /,
        ;; we can indicate what would be run.
        (let ((task-info (format "Task created: %s\nSubagent: %s\nPrompt: %s"
                                  description subagent-type prompt)))
          (puthash task-id
                   (list :description description
                         :prompt prompt
                         :subagent-type subagent-type
                         :status "recorded"
                         :created (current-time))
                   mcp-server-emacs-tools--task-registry)
          (format "task_id: %s (for resuming to continue this task if needed)\n\n<task_result>\n%s\n\nNote: This Emacs MCP implementation records the task. Integrate with your AI backend to execute subagent tasks.\n</task_result>"
                  task-id task-info)))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "task"
  :title "Launch Task"
  :description "Launch a new agent to handle complex, multistep tasks autonomously.

When to use the Task tool:
- When you are instructed to execute custom slash commands
- For open-ended research and investigation tasks
- When a task is complex enough to warrant a dedicated subagent

When NOT to use the Task tool:
- If you want to read a specific file path, use the Read or Glob tool instead
- If you are searching for specific code, use the Grep tool instead
- For simple, direct operations

Usage notes:
1. Launch multiple agents concurrently whenever possible
2. Each agent invocation starts fresh unless you provide task_id to resume
3. The output includes a task_id you can reuse to continue the same session"
  :input-schema '((type . "object")
                  (properties . ((description . ((type . "string")
                                                 (description . "A short (3-5 words) description of the task")))
                                 (prompt . ((type . "string")
                                            (description . "The task for the agent to perform")))
                                 (subagent_type . ((type . "string")
                                                   (description . "The type of specialized agent to use for this task")))
                                 (task_id . ((type . "string")
                                             (description . "Set to resume a previous task session")))))
                  (required . ["description" "prompt" "subagent_type"]))
  :function #'mcp-server-emacs-tools--task-handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . :false)
                 (idempotentHint . :false)
                 (openWorldHint . t))))

(provide 'mcp-server-emacs-tools-task)

;;; mcp-server-emacs-tools-task.el ends here
