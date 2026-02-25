;;; mcp-server-emacs-tools-todo.el --- Todo Management MCP Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tools for reading and writing a session todo list.
;; Mirrors the opencode todo.ts tool.
;; Todos are stored in-memory per Emacs session (not persisted across restarts).
;; For persistence, set `mcp-server-emacs-tools-todo-file'.

;;; Code:

(require 'mcp-server-tools)
(require 'json)
(require 'cl-lib)

(defvar mcp-server-emacs-tools--todo-list '()
  "Current session todo list. Each item is an alist with id, content, status.")

(defcustom mcp-server-emacs-tools-todo-file nil
  "If non-nil, path to a JSON file for persisting todos across sessions."
  :type '(choice (const nil) file)
  :group 'mcp-server-emacs-tools)

(defun mcp-server-emacs-tools--todo-load ()
  "Load todos from file if configured."
  (when (and mcp-server-emacs-tools-todo-file
             (file-exists-p mcp-server-emacs-tools-todo-file))
    (condition-case nil
        (setq mcp-server-emacs-tools--todo-list
              (let ((json-array-type 'list)
                    (json-object-type 'alist))
                (json-read-file mcp-server-emacs-tools-todo-file)))
      (error nil))))

(defun mcp-server-emacs-tools--todo-save ()
  "Save todos to file if configured."
  (when mcp-server-emacs-tools-todo-file
    (condition-case nil
        (write-region (json-encode mcp-server-emacs-tools--todo-list)
                      nil mcp-server-emacs-tools-todo-file nil 'silent)
      (error nil))))

(defun mcp-server-emacs-tools--todo-validate-status (status)
  "Return non-nil if STATUS is a valid todo status string."
  (member status '("pending" "in_progress" "completed" "cancelled")))

(defun mcp-server-emacs-tools--todowrite-handler (args)
  "Handle todowrite tool invocation with ARGS."
  (condition-case err
      (let* ((todos-raw (alist-get 'todos args))
             (todos (if (vectorp todos-raw) (append todos-raw nil) todos-raw)))
        (unless todos (error "todos is required"))
        ;; Validate each todo
        (dolist (todo todos)
          (unless (alist-get 'id todo) (error "Each todo must have an id"))
          (unless (alist-get 'content todo) (error "Each todo must have content"))
          (let ((status (alist-get 'status todo)))
            (when (and status (not (mcp-server-emacs-tools--todo-validate-status status)))
              (error "Invalid status: %s. Must be pending, in_progress, completed, or cancelled" status))))
        ;; Update the todo list
        (setq mcp-server-emacs-tools--todo-list
              (mapcar (lambda (todo)
                        (list (cons 'id (alist-get 'id todo))
                              (cons 'content (alist-get 'content todo))
                              (cons 'status (or (alist-get 'status todo) "pending"))))
                      todos))
        (mcp-server-emacs-tools--todo-save)
        (let ((active (cl-count-if (lambda (todo)
                                     (not (member (alist-get 'status todo)
                                                  '("completed" "cancelled"))))
                                   mcp-server-emacs-tools--todo-list)))
          (format "Updated %d todos (%d active).\n\n%s"
                  (length mcp-server-emacs-tools--todo-list)
                  active
                  (json-encode mcp-server-emacs-tools--todo-list))))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-server-emacs-tools--todoread-handler (_args)
  "Handle todoread tool invocation."
  (condition-case err
      (progn
        (mcp-server-emacs-tools--todo-load)
        (json-encode mcp-server-emacs-tools--todo-list))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "todowrite"
  :title "Todo Write"
  :description "Use this tool to create and manage a structured task list for your current coding session. This helps track progress, organize complex tasks, and demonstrate thoroughness.

When to Use:
- Complex multi-step tasks (3+ distinct steps)
- Non-trivial tasks requiring careful planning
- User explicitly requests todo list
- User provides multiple tasks (numbered/comma-separated)
- After receiving new instructions - capture requirements as todos
- After completing tasks - mark complete and add follow-ups

Task States: pending, in_progress (only one at a time), completed, cancelled"
  :input-schema '((type . "object")
                  (properties . ((todos . ((type . "array")
                                           (items . ((type . "object")
                                                     (properties . ((id . ((type . "string")
                                                                           (description . "Unique identifier")))
                                                                    (content . ((type . "string")
                                                                                (description . "The task description")))
                                                                    (status . ((type . "string")
                                                                               (enum . ("pending" "in_progress" "completed" "cancelled"))
                                                                               (description . "Current status")))))
                                                     (required . ["id" "content" "status"])))
                                           (description . "The updated todo list")))))
                  (required . ["todos"]))
  :function #'mcp-server-emacs-tools--todowrite-handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . :false)
                 (idempotentHint . :false)
                 (openWorldHint . :false))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "todoread"
  :title "Todo Read"
  :description "Use this tool to read the current to-do list for the session. Use it frequently to track status.

Usage:
- This tool takes no parameters.
- Returns a list of todo items with their status and content."
  :input-schema `((type . "object")
                  (properties . ,(make-hash-table))
                  (additionalProperties . :false))
  :function #'mcp-server-emacs-tools--todoread-handler
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false)
                 (idempotentHint . t)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-todo)

;;; mcp-server-emacs-tools-todo.el ends here
