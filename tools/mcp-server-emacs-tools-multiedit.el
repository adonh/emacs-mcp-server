;;; mcp-server-emacs-tools-multiedit.el --- Multi-Edit MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for making multiple sequential edits to a single file.
;; Built on top of the edit tool. Mirrors the opencode multiedit.ts tool.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-edit)
(require 'cl-lib)

(defun mcp-server-emacs-tools--multiedit-handler (args)
  "Handle multiedit tool invocation with ARGS."
  (condition-case err
      (let* ((filepath (alist-get 'filePath args))
             (edits-raw (alist-get 'edits args))
             (edits (if (vectorp edits-raw) (append edits-raw nil) edits-raw)))
        (unless filepath (error "filePath is required"))
        (unless edits (error "edits is required"))
        (let ((filepath (expand-file-name filepath))
              (last-output nil))
          ;; Apply each edit sequentially
          (dolist (edit edits)
            (let* ((old-string (alist-get 'oldString edit))
                   (new-string (alist-get 'newString edit))
                   (replace-all (alist-get 'replaceAll edit))
                   (result (mcp-server-emacs-tools--edit-handler
                            (list (cons 'filePath filepath)
                                  (cons 'oldString old-string)
                                  (cons 'newString new-string)
                                  (cons 'replaceAll replace-all)))))
              (when (string-prefix-p "Error:" result)
                (error "Edit failed: %s" result))
              (setq last-output result)))
          (or last-output "All edits applied successfully.")))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "multiedit"
  :title "Multi-Edit File"
  :description "This is a tool for making multiple edits to a single file in one operation. Built on top of the Edit tool. Prefer this tool over the Edit tool when you need to make multiple edits to the same file.

Before using this tool:
1. Use the Read tool to understand the file's contents and context
2. Verify the directory path is correct

IMPORTANT:
- All edits are applied in sequence, in the order they are provided
- Each edit operates on the result of the previous edit
- All edits must be valid for the operation to succeed"
  :input-schema '((type . "object")
                  (properties . ((filePath . ((type . "string")
                                              (description . "The absolute path to the file to modify")))
                                 (edits . ((type . "array")
                                           (items . ((type . "object")
                                                     (properties . ((oldString . ((type . "string")
                                                                                  (description . "The text to replace")))
                                                                    (newString . ((type . "string")
                                                                                  (description . "The text to replace it with")))
                                                                    (replaceAll . ((type . "boolean")
                                                                                   (description . "Replace all occurrences (default false)")))))
                                                     (required . ["oldString" "newString"])))
                                           (description . "Array of edit operations to perform sequentially on the file")))))
                  (required . ["filePath" "edits"]))
  :function #'mcp-server-emacs-tools--multiedit-handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . t)
                 (idempotentHint . :false)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-multiedit)

;;; mcp-server-emacs-tools-multiedit.el ends here
