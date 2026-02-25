;;; mcp-server-emacs-tools-ls.el --- List Directory MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for listing directory contents as a tree.
;; Mirrors the opencode ls.ts tool with ignore patterns and tree rendering.

;;; Code:

(require 'mcp-server-tools)
(require 'cl-lib)

(defconst mcp-server-emacs-tools--ls-limit 100
  "Maximum number of files to include in listing.")

(defconst mcp-server-emacs-tools--ls-ignore-patterns
  '("node_modules" "__pycache__" ".git" "dist" "build" "target"
    "vendor" "bin" "obj" ".idea" ".vscode" ".zig-cache" "zig-out"
    ".coverage" "coverage" "tmp" "temp" ".cache" "cache" "logs"
    ".venv" "venv" "env")
  "Directory names to ignore by default.")

(defun mcp-server-emacs-tools--ls-should-ignore-p (name ignore-extra)
  "Return non-nil if NAME matches default or IGNORE-EXTRA ignore patterns."
  (or (member name mcp-server-emacs-tools--ls-ignore-patterns)
      (and ignore-extra
           (cl-some (lambda (pat)
                      (string-match-p (wildcard-to-regexp pat) name))
                    ignore-extra))))

(defun mcp-server-emacs-tools--ls-collect-files (root ignore-extra)
  "Collect up to `mcp-server-emacs-tools--ls-limit' files under ROOT.
Returns list of relative paths, ignoring IGNORE-EXTRA patterns."
  (let ((files '())
        (count 0)
        (dirs-to-visit (list "")))
    (while (and dirs-to-visit (< count mcp-server-emacs-tools--ls-limit))
      (let* ((rel-dir (pop dirs-to-visit))
             (abs-dir (if (string= rel-dir "") root (expand-file-name rel-dir root)))
             (entries (condition-case nil
                          (directory-files abs-dir nil nil t)
                        (error nil))))
        (dolist (entry (sort entries #'string<))
          (unless (member entry '("." ".."))
            (let* ((rel-path (if (string= rel-dir "") entry (concat rel-dir "/" entry)))
                   (abs-path (expand-file-name entry abs-dir)))
              (cond
               ((mcp-server-emacs-tools--ls-should-ignore-p entry ignore-extra)
                nil)  ; skip
               ((file-directory-p abs-path)
                ;; Add to dirs to visit (at end to do BFS)
                (setq dirs-to-visit (append dirs-to-visit (list rel-path))))
               (t
                (push rel-path files)
                (cl-incf count)
                (when (>= count mcp-server-emacs-tools--ls-limit)
                  (cl-return)))))))))
    (nreverse files)))

(defun mcp-server-emacs-tools--ls-render-tree (root files)
  "Render FILES (relative paths under ROOT) as an indented tree string."
  (let ((dirs (make-hash-table :test 'equal))
        (files-by-dir (make-hash-table :test 'equal)))
    ;; Populate structure
    (dolist (file files)
      (let* ((dir (or (file-name-directory file) ""))
             (dir (if (string-suffix-p "/" dir) (substring dir 0 -1) dir))
             (dir (if (string= dir "") "." dir))
             (base (file-name-nondirectory file)))
        ;; Add all parent directories
        (let ((parts (if (string= dir ".") '()
                       (split-string dir "/" t))))
          (cl-loop for i from 0 to (length parts) do
            (let ((d (if (= i 0) "."
                       (mapconcat #'identity (cl-subseq parts 0 i) "/"))))
              (puthash d t dirs))))
        (puthash "." t dirs)
        (let ((existing (gethash dir files-by-dir)))
          (puthash dir (append existing (list base)) files-by-dir))))
    ;; Render recursively
    (let ((output ""))
      (cl-labels
          ((render-dir (dir-path depth)
             (when (> depth 0)
               (let* ((indent (make-string (* depth 2) ? ))
                      (name (file-name-nondirectory dir-path)))
                 (setq output (concat output indent name "/\n"))))
             (let* ((child-indent (make-string (* (1+ depth) 2) ? ))
                    ;; Subdirectories
                    (subdirs (sort (cl-remove-if-not
                                    (lambda (d)
                                      (and (not (string= d dir-path))
                                           (not (string= d "."))
                                           (string= (let ((parent (file-name-directory d)))
                                                       (if parent
                                                           (if (string-suffix-p "/" parent)
                                                               (substring parent 0 -1)
                                                             parent)
                                                         "."))
                                                    dir-path)))
                                    (hash-table-keys dirs))
                                   #'string<))
                    ;; Files in this dir
                    (dir-files (sort (gethash dir-path files-by-dir) #'string<)))
               ;; Render subdirs first
               (dolist (subdir subdirs)
                 (render-dir subdir (1+ depth)))
               ;; Then files
               (dolist (f dir-files)
                 (setq output (concat output child-indent f "\n"))))))
        (render-dir "." 0))
      output)))

(defun mcp-server-emacs-tools--ls-handler (args)
  "Handle ls (list) tool invocation with ARGS."
  (condition-case err
      (let* ((search-path (alist-get 'path args))
             (ignore-extra (let ((v (alist-get 'ignore args)))
                             (when (vectorp v) (append v nil))))
             (search-path (expand-file-name (or search-path default-directory))))
        (unless (file-directory-p search-path)
          (error "Not a directory: %s" search-path))
        (let* ((files (mcp-server-emacs-tools--ls-collect-files search-path ignore-extra))
               (truncated (= (length files) mcp-server-emacs-tools--ls-limit))
               (tree (mcp-server-emacs-tools--ls-render-tree search-path files)))
          (concat search-path "/\n" tree
                  (when truncated
                    (format "\n(Results truncated at %d files)\n"
                            mcp-server-emacs-tools--ls-limit)))))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "list"
  :title "List Directory"
  :description "Lists files and directories in a given path. The path parameter must be absolute; omit it to use the current workspace directory. You can optionally provide an array of glob patterns to ignore with the ignore parameter. You should generally prefer the Glob and Grep tools, if you know which directories to search."
  :input-schema '((type . "object")
                  (properties . ((path . ((type . "string")
                                          (description . "The absolute path to the directory to list (must be absolute, not relative)")))
                                 (ignore . ((type . "array")
                                            (items . ((type . "string")))
                                            (description . "List of glob patterns to ignore")))))
                  (required . []))
  :function #'mcp-server-emacs-tools--ls-handler
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false)
                 (idempotentHint . t)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-ls)

;;; mcp-server-emacs-tools-ls.el ends here
