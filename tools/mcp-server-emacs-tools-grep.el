;;; mcp-server-emacs-tools-grep.el --- Grep MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for searching file contents with regex.
;; Uses ripgrep (rg) if available, falls back to Emacs grep.
;; Mirrors the opencode grep.ts tool.

;;; Code:

(require 'mcp-server-tools)
(require 'cl-lib)

(defconst mcp-server-emacs-tools--grep-max-line-length 2000
  "Maximum characters per result line before truncation.")

(defconst mcp-server-emacs-tools--grep-result-limit 100
  "Maximum number of matches to return.")

(defun mcp-server-emacs-tools--grep-find-rg ()
  "Return path to ripgrep executable, or nil if not found."
  (or (executable-find "rg")
      (executable-find "ripgrep")))

(defun mcp-server-emacs-tools--grep-with-rg (pattern search-path include)
  "Run ripgrep for PATTERN in SEARCH-PATH, filtering by INCLUDE glob.
Returns a list of (file line-num line-text mod-time) plists."
  (let* ((rg (mcp-server-emacs-tools--grep-find-rg))
         (args (list "-nH" "--hidden" "--no-messages"
                     "--field-match-separator=|"
                     "--regexp" pattern))
         (args (if include (append args (list "--glob" include)) args))
         (args (append args (list search-path)))
         (output (with-temp-buffer
                   (apply #'call-process rg nil t nil args)
                   (buffer-string)))
         (matches '()))
    (dolist (line (split-string output "\n" t))
      (let* ((parts (split-string line "|" nil))
             (file (car parts))
             (line-num-str (cadr parts))
             (text (mapconcat #'identity (cddr parts) "|")))
        (when (and file line-num-str)
          (let ((line-num (string-to-number line-num-str))
                (mtime (nth 5 (file-attributes file))))
            (push (list :path file
                        :line line-num
                        :text text
                        :mtime (if mtime (float-time mtime) 0))
                  matches)))))
    (nreverse matches)))

(defun mcp-server-emacs-tools--grep-with-grep (pattern search-path include)
  "Fallback: use Emacs grep for PATTERN in SEARCH-PATH filtered by INCLUDE.
Returns same format as rg variant."
  (let* ((name-pattern (or include "*"))
         (args (list "-rn" "--include" name-pattern
                     "-E" pattern search-path))
         (output (with-temp-buffer
                   (apply #'call-process "grep" nil t nil args)
                   (buffer-string)))
         (matches '()))
    (dolist (line (split-string output "\n" t))
      (when (string-match "^\\([^:]+\\):\\([0-9]+\\):\\(.*\\)$" line)
        (let* ((file (match-string 1 line))
               (line-num (string-to-number (match-string 2 line)))
               (text (match-string 3 line))
               (mtime (nth 5 (file-attributes file))))
          (push (list :path file
                      :line line-num
                      :text text
                      :mtime (if mtime (float-time mtime) 0))
                matches))))
    (nreverse matches)))

(defun mcp-server-emacs-tools--grep-handler (args)
  "Handle grep tool invocation with ARGS."
  (condition-case err
      (let* ((pattern (alist-get 'pattern args))
             (search-path (alist-get 'path args))
             (include (alist-get 'include args)))
        (unless pattern (error "pattern is required"))
        (let* ((search-path (expand-file-name (or search-path default-directory)))
               (matches (if (mcp-server-emacs-tools--grep-find-rg)
                            (mcp-server-emacs-tools--grep-with-rg pattern search-path include)
                          (mcp-server-emacs-tools--grep-with-grep pattern search-path include))))
          (if (null matches)
              "No files found"
            ;; Sort by modification time descending
            (setq matches (sort matches (lambda (a b)
                                          (> (plist-get a :mtime) (plist-get b :mtime)))))
            (let* ((total (length matches))
                   (truncated (> total mcp-server-emacs-tools--grep-result-limit))
                   (final (if truncated
                              (cl-subseq matches 0 mcp-server-emacs-tools--grep-result-limit)
                            matches))
                   (output-lines (list (format "Found %d matches%s"
                                               total
                                               (if truncated
                                                   (format " (showing first %d)"
                                                           mcp-server-emacs-tools--grep-result-limit)
                                                 ""))))
                   (current-file nil))
              (dolist (m final)
                (let ((file (plist-get m :path))
                      (line-num (plist-get m :line))
                      (text (plist-get m :text)))
                  (unless (equal file current-file)
                    (when current-file (push "" output-lines))
                    (setq current-file file)
                    (push (format "%s:" file) output-lines))
                  (let ((trunc-text (if (> (length text) mcp-server-emacs-tools--grep-max-line-length)
                                        (concat (substring text 0 mcp-server-emacs-tools--grep-max-line-length) "...")
                                      text)))
                    (push (format "  Line %d: %s" line-num trunc-text) output-lines))))
              (when truncated
                (push "" output-lines)
                (push (format "(Results truncated: showing %d of %d matches (%d hidden). Consider a more specific path or pattern.)"
                              mcp-server-emacs-tools--grep-result-limit total
                              (- total mcp-server-emacs-tools--grep-result-limit))
                      output-lines))
              (mapconcat #'identity (nreverse output-lines) "\n")))))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "grep"
  :title "Grep"
  :description "Fast content search tool that works with any codebase size.
- Searches file contents using regular expressions
- Supports full regex syntax (eg. \"log.*Error\", \"function\\s+\\w+\", etc.)
- Filter files by pattern with the include parameter (eg. \"*.js\", \"*.{ts,tsx}\")
- Returns file paths and line numbers with at least one match sorted by modification time
- Use this tool when you need to find files containing specific patterns"
  :input-schema '((type . "object")
                  (properties . ((pattern . ((type . "string")
                                             (description . "The regex pattern to search for in file contents")))
                                 (path . ((type . "string")
                                          (description . "The directory to search in. Defaults to the current working directory.")))
                                 (include . ((type . "string")
                                             (description . "File pattern to include in the search (e.g. \"*.js\", \"*.{ts,tsx}\")")))))
                  (required . ["pattern"]))
  :function #'mcp-server-emacs-tools--grep-handler
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false)
                 (idempotentHint . t)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-grep)

;;; mcp-server-emacs-tools-grep.el ends here
