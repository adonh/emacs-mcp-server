;;; mcp-server-emacs-tools-bash.el --- Bash Shell Execution MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for executing shell commands.
;; Mirrors the opencode bash.ts tool with timeout support and working directory.

;;; Code:

(require 'mcp-server-tools)
(require 'cl-lib)

(defconst mcp-server-emacs-tools--bash-default-timeout 120000
  "Default timeout in milliseconds (2 minutes).")

(defconst mcp-server-emacs-tools--bash-max-output-length 30000
  "Maximum output characters to include in result metadata.")

(defun mcp-server-emacs-tools--bash-run (command workdir timeout-ms)
  "Run COMMAND in WORKDIR with TIMEOUT-MS millisecond timeout.
Returns (exit-code output timed-out)."
  (let* ((shell (or (getenv "SHELL") "/bin/bash"))
         (output-buffer (generate-new-buffer " *mcp-bash-output*"))
         (timeout-secs (/ timeout-ms 1000.0))
         (default-directory (or workdir default-directory))
         (proc nil)
         (exit-code nil)
         (timed-out nil))
    (unwind-protect
        (progn
          (setq proc (start-process-shell-command
                      "mcp-bash" output-buffer command))
          (let ((deadline (+ (float-time) timeout-secs)))
            (while (and (process-live-p proc)
                        (< (float-time) deadline))
              (accept-process-output proc 0.1))
            (when (process-live-p proc)
              (setq timed-out t)
              (kill-process proc)
              ;; Give it a moment to clean up
              (sit-for 0.2)))
          (setq exit-code (process-exit-status proc))
          (let ((output (with-current-buffer output-buffer
                          (buffer-string))))
            (list exit-code output timed-out)))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))

(defun mcp-server-emacs-tools--bash-handler (args)
  "Handle bash tool invocation with ARGS."
  (condition-case err
      (let* ((command (alist-get 'command args))
             (timeout (alist-get 'timeout args))
             (workdir (alist-get 'workdir args))
             (description (alist-get 'description args)))
        (unless command (error "command is required"))
        (when (and timeout (< timeout 0))
          (error "Invalid timeout value: %s. Timeout must be a positive number." timeout))
        (let* ((timeout-ms (or timeout mcp-server-emacs-tools--bash-default-timeout))
               (workdir (when workdir (expand-file-name workdir)))
               (result (mcp-server-emacs-tools--bash-run command workdir timeout-ms))
               (exit-code (nth 0 result))
               (output (nth 1 result))
               (timed-out (nth 2 result))
               (extra (cond
                       (timed-out (format "\n\n<bash_metadata>\nbash tool terminated command after exceeding timeout %d ms\n</bash_metadata>" timeout-ms))
                       (t ""))))
          (concat output extra)))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "bash"
  :title "Bash"
  :description "Executes a given bash command in a shell session with optional timeout, ensuring proper handling and security measures.

IMPORTANT: This tool is for terminal operations like git, npm, docker, etc. DO NOT use it for file operations (reading, writing, editing, searching, finding files) - use the specialized tools for this instead.

Usage notes:
- The command argument is required.
- You can specify an optional timeout in milliseconds. If not specified, commands will time out after 120000ms (2 minutes).
- It is very helpful if you write a clear, concise description of what this command does in 5-10 words.
- Avoid using Bash with find, grep, cat, head, tail, sed, awk, or echo unless truly necessary.
- Use workdir instead of cd commands.

When issuing multiple commands:
- If the commands are independent and can run in parallel, make multiple Bash tool calls in a single message.
- If the commands depend on each other and must run sequentially, use && to chain them."
  :input-schema '((type . "object")
                  (properties . ((command . ((type . "string")
                                             (description . "The command to execute")))
                                 (timeout . ((type . "number")
                                             (description . "Optional timeout in milliseconds")))
                                 (workdir . ((type . "string")
                                             (description . "The working directory to run the command in. Use this instead of 'cd' commands.")))
                                 (description . ((type . "string")
                                                 (description . "Clear, concise description of what this command does in 5-10 words.")))))
                  (required . ["command" "description"]))
  :function #'mcp-server-emacs-tools--bash-handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . t)
                 (idempotentHint . :false)
                 (openWorldHint . t))))

(provide 'mcp-server-emacs-tools-bash)

;;; mcp-server-emacs-tools-bash.el ends here
