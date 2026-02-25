;;; mcp-server-emacs-tools-glob.el --- Glob File Search MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for finding files by glob pattern.
;; Uses ripgrep --files if available, falls back to find command.
;; Mirrors the opencode glob.ts tool.

;;; Code:

(require 'mcp-server-tools)
(require 'cl-lib)

(defconst mcp-server-emacs-tools--glob-result-limit 100
  "Maximum number of files to return.")

(defun mcp-server-emacs-tools--glob-find-rg ()
  "Return ripgrep path or nil."
  (executable-find "rg"))

(defun mcp-server-emacs-tools--glob-with-rg (pattern search-path)
  "Use ripgrep to find files matching PATTERN in SEARCH-PATH.
Returns list of absolute paths sorted by modification time."
  (let* ((rg (mcp-server-emacs-tools--glob-find-rg))
         (output (with-temp-buffer
                   (call-process rg nil t nil "--files" "--hidden"
                                 "--glob" pattern search-path)
                   (buffer-string)))
         (files '()))
    (dolist (rel (split-string output "\n" t))
      (let* ((full (expand-file-name rel search-path))
             (mtime (nth 5 (file-attributes full))))
        (push (list :path full :mtime (if mtime (float-time mtime) 0)) files)))
    files))

(defun mcp-server-emacs-tools--glob-with-find (pattern search-path)
  "Use find command to locate files matching PATTERN in SEARCH-PATH."
  (let* ((output (with-temp-buffer
                   (call-process "find" nil t nil search-path "-name" pattern "-type" "f")
                   (buffer-string)))
         (files '()))
    (dolist (full (split-string output "\n" t))
      (let ((mtime (nth 5 (file-attributes full))))
        (push (list :path full :mtime (if mtime (float-time mtime) 0)) files)))
    files))

(defun mcp-server-emacs-tools--glob-handler (args)
  "Handle glob tool invocation with ARGS."
  (condition-case err
      (let* ((pattern (alist-get 'pattern args))
             (search-path (alist-get 'path args)))
        (unless pattern (error "pattern is required"))
        (let* ((search-path (expand-file-name (or search-path default-directory)))
               (files (if (mcp-server-emacs-tools--glob-find-rg)
                          (mcp-server-emacs-tools--glob-with-rg pattern search-path)
                        (mcp-server-emacs-tools--glob-with-find pattern search-path))))
          ;; Sort by modification time descending
          (setq files (sort files (lambda (a b) (> (plist-get a :mtime) (plist-get b :mtime)))))
          (let* ((truncated (> (length files) mcp-server-emacs-tools--glob-result-limit))
                 (final (if truncated
                            (cl-subseq files 0 mcp-server-emacs-tools--glob-result-limit)
                          files))
                 (paths (mapcar (lambda (f) (plist-get f :path)) final)))
            (if (null paths)
                "No files found"
              (concat (mapconcat #'identity paths "\n")
                      (if truncated
                          (format "\n\n(Results are truncated: showing first %d results. Consider using a more specific path or pattern.)"
                                  mcp-server-emacs-tools--glob-result-limit)
                        ""))))))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "glob"
  :title "Glob"
  :description "Fast file pattern matching tool that works with any codebase size.
- Supports glob patterns like \"**/*.js\" or \"src/**/*.ts\"
- Returns matching file paths sorted by modification time
- Use this tool when you need to find files by name patterns
- You have the capability to call multiple tools in a single response. It is always better to speculatively perform multiple searches as a batch."
  :input-schema '((type . "object")
                  (properties . ((pattern . ((type . "string")
                                             (description . "The glob pattern to match files against")))
                                 (path . ((type . "string")
                                          (description . "The directory to search in. If not specified, the current working directory will be used. DO NOT enter \"undefined\" or \"null\" - simply omit it for the default behavior.")))))
                  (required . ["pattern"]))
  :function #'mcp-server-emacs-tools--glob-handler
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false)
                 (idempotentHint . t)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-glob)

;;; mcp-server-emacs-tools-glob.el ends here
