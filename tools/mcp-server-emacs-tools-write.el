;;; mcp-server-emacs-tools-write.el --- Write File MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for writing files.
;; Mirrors the opencode write.ts tool: writes content to files,
;; creating parent directories as needed.

;;; Code:

(require 'mcp-server-tools)

(defun mcp-server-emacs-tools--write-handler (args)
  "Handle write tool invocation with ARGS."
  (condition-case err
      (let* ((filepath (alist-get 'filePath args))
             (content (alist-get 'content args)))
        (unless filepath
          (error "filePath is required"))
        (unless content
          (error "content is required"))
        (let* ((filepath (expand-file-name filepath))
               (dir (file-name-directory filepath))
               (existed (file-exists-p filepath)))
          ;; Create parent directories if needed
          (when (and dir (not (file-directory-p dir)))
            (make-directory dir t))
          ;; Write the file
          (write-region content nil filepath nil 'silent)
          (format "Wrote file successfully: %s%s"
                  filepath
                  (if existed "" " (new file)"))))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "write"
  :title "Write File"
  :description "Writes a file to the local filesystem.

Usage:
- This tool will overwrite the existing file if there is one at the provided path.
- ALWAYS prefer editing existing files in the codebase. NEVER write new files unless explicitly required.
- NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested.
- Only use emojis if the user explicitly requests it."
  :input-schema '((type . "object")
                  (properties . ((filePath . ((type . "string")
                                              (description . "The absolute path to the file to write (must be absolute, not relative)")))
                                 (content . ((type . "string")
                                             (description . "The content to write to the file")))))
                  (required . ["filePath" "content"]))
  :function #'mcp-server-emacs-tools--write-handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . t)
                 (idempotentHint . :false)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-write)

;;; mcp-server-emacs-tools-write.el ends here
