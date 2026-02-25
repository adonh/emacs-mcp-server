;;; mcp-server-emacs-tools-apply-patch.el --- Apply Patch MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for applying multi-file patches in opencode's custom patch format.
;; Mirrors the opencode apply_patch.ts tool.
;;
;; Patch format:
;;   *** Begin Patch
;;   *** Add File: <path>
;;   +line1
;;   +line2
;;   *** Update File: <path>
;;   *** Move to: <new-path>        (optional)
;;   @@ context-line
;;   -removed-line
;;   +added-line
;;   *** Delete File: <path>
;;   *** End Patch

;;; Code:

(require 'mcp-server-tools)
(require 'cl-lib)

;;; Patch Parser

(defun mcp-server-emacs-tools--patch-parse (patch-text)
  "Parse PATCH-TEXT into a list of file operation plists.
Each plist has :type, :path, and type-specific fields."
  (let* ((lines (split-string patch-text "\n" nil))
         (i 0)
         (len (length lines))
         (hunks '()))
    ;; Find *** Begin Patch
    (while (and (< i len)
                (not (string= (string-trim (nth i lines)) "*** Begin Patch")))
      (cl-incf i))
    (unless (< i len) (error "No '*** Begin Patch' found"))
    (cl-incf i) ; skip the Begin Patch line
    ;; Parse file sections
    (while (and (< i len)
                (not (string= (string-trim (nth i lines)) "*** End Patch")))
      (let ((line (nth i lines)))
        (cond
         ;; Add File
         ((string-match "^\\*\\*\\* Add File: \\(.+\\)$" line)
          (let ((path (match-string 1 line))
                (content-lines '()))
            (cl-incf i)
            (while (and (< i len)
                        (not (string-match "^\\*\\*\\*" (nth i lines))))
              (let ((l (nth i lines)))
                (when (string-prefix-p "+" l)
                  (push (substring l 1) content-lines)))
              (cl-incf i))
            (push (list :type 'add
                        :path path
                        :contents (mapconcat #'identity (nreverse content-lines) "\n"))
                  hunks)))
         ;; Delete File
         ((string-match "^\\*\\*\\* Delete File: \\(.+\\)$" line)
          (push (list :type 'delete :path (match-string 1 line)) hunks)
          (cl-incf i))
         ;; Update File
         ((string-match "^\\*\\*\\* Update File: \\(.+\\)$" line)
          (let ((path (match-string 1 line))
                (move-path nil)
                (chunks '())
                (current-chunk nil))
            (cl-incf i)
            ;; Check for optional Move to
            (when (and (< i len)
                       (string-match "^\\*\\*\\* Move to: \\(.+\\)$" (nth i lines)))
              (setq move-path (match-string 1 (nth i lines)))
              (cl-incf i))
            ;; Parse @@ sections
            (while (and (< i len)
                        (not (string-match "^\\*\\*\\*" (nth i lines))))
              (let ((l (nth i lines)))
                (cond
                 ((string-prefix-p "@@" l)
                  ;; New hunk context line
                  (when current-chunk (push (nreverse current-chunk) chunks))
                  (setq current-chunk (list (list :type 'context
                                                  :line (string-trim (substring l 2))))))
                 ((string-prefix-p "-" l)
                  (push (list :type 'remove :line (substring l 1)) current-chunk))
                 ((string-prefix-p "+" l)
                  (push (list :type 'insert :line (substring l 1)) current-chunk))
                 ((string-prefix-p " " l)
                  (push (list :type 'context :line (substring l 1)) current-chunk))
                 (t
                  ;; Plain context line
                  (push (list :type 'context :line l) current-chunk))))
              (cl-incf i))
            (when current-chunk (push (nreverse current-chunk) chunks))
            (push (list :type 'update
                        :path path
                        :move-path move-path
                        :chunks (nreverse chunks))
                  hunks)))
         (t (cl-incf i)))))
    (nreverse hunks)))

;;; Patch Application

(defun mcp-server-emacs-tools--patch-apply-update (content chunks)
  "Apply update CHUNKS to CONTENT string. Returns new content."
  (let ((lines (split-string content "\n" nil))
         (result-lines (copy-sequence (split-string content "\n" nil))))
    (dolist (chunk chunks)
      (let* ((ops (if (listp (car chunk)) chunk (list chunk)))
             ;; Find the context anchor
             (context-line (alist-get 'line (car (seq-filter (lambda (op) (eq (alist-get 'type op) 'context)) ops))))
             (context-lines (seq-filter (lambda (op) (eq (alist-get 'type op) 'context)) ops))
             (remove-lines (mapcar (lambda (op) (alist-get 'line op))
                                   (seq-filter (lambda (op) (eq (alist-get 'type op) 'remove)) ops)))
             (insert-lines (mapcar (lambda (op) (alist-get 'line op))
                                   (seq-filter (lambda (op) (eq (alist-get 'type op) 'insert)) ops))))
        ;; Simple line-by-line replacement: find and replace
        (when (and remove-lines (car context-lines))
          (let* ((anchor (alist-get 'line (car context-lines)))
                 (pos (cl-position anchor result-lines :test #'string=)))
            (when pos
              (let ((new-lines (append
                                (cl-subseq result-lines 0 pos)
                                insert-lines
                                (cl-remove-if (lambda (l) (member l remove-lines))
                                              (cl-subseq result-lines pos)))))
                (setq result-lines new-lines)))))))
    (mapconcat #'identity result-lines "\n")))

(defun mcp-server-emacs-tools--apply-patch-handler (args)
  "Handle apply_patch tool invocation with ARGS."
  (condition-case err
      (let* ((patch-text (alist-get 'patchText args)))
        (unless patch-text (error "patchText is required"))
        (let* ((hunks (mcp-server-emacs-tools--patch-parse patch-text))
               (summary-lines '()))
          (when (null hunks) (error "No hunks found in patch"))
          (dolist (hunk hunks)
            (let* ((type (plist-get hunk :type))
                   (path (expand-file-name (plist-get hunk :path))))
              (pcase type
                ('add
                 (let* ((dir (file-name-directory path))
                        (contents (plist-get hunk :contents))
                        (contents (if (or (string= contents "")
                                          (string-suffix-p "\n" contents))
                                      contents
                                    (concat contents "\n"))))
                   (when dir (make-directory dir t))
                   (write-region contents nil path nil 'silent)
                   (push (format "A %s" (plist-get hunk :path)) summary-lines)))
                ('delete
                 (when (file-exists-p path)
                   (delete-file path))
                 (push (format "D %s" (plist-get hunk :path)) summary-lines))
                ('update
                 (unless (file-exists-p path)
                   (error "File to update not found: %s" path))
                 (let* ((old-content (with-temp-buffer
                                       (insert-file-contents path)
                                       (buffer-string)))
                        (new-content (mcp-server-emacs-tools--patch-apply-update
                                      old-content (plist-get hunk :chunks)))
                        (move-path (when (plist-get hunk :move-path)
                                     (expand-file-name (plist-get hunk :move-path)))))
                   (if move-path
                       (progn
                         (make-directory (file-name-directory move-path) t)
                         (write-region new-content nil move-path nil 'silent)
                         (delete-file path)
                         (push (format "M %s -> %s"
                                       (plist-get hunk :path)
                                       (plist-get hunk :move-path))
                               summary-lines))
                     (write-region new-content nil path nil 'silent)
                     (push (format "M %s" (plist-get hunk :path)) summary-lines)))))))
          (format "Success. Updated the following files:\n%s"
                  (mapconcat #'identity (nreverse summary-lines) "\n"))))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "apply_patch"
  :title "Apply Patch"
  :description "Use the apply_patch tool to edit files. Your patch language is a stripped-down, file-oriented diff format.

Format:
*** Begin Patch
[ one or more file sections ]
*** End Patch

File operations:
*** Add File: <path> - create a new file. Every following line is a + line.
*** Delete File: <path> - remove an existing file. Nothing follows.
*** Update File: <path> - patch an existing file in place (optionally with a rename).
*** Move to: <new-path> - optional rename after Update File header

Example:
*** Begin Patch
*** Add File: hello.txt
+Hello world
*** Update File: src/app.py
@@ def greet():
-print(\"Hi\")
+print(\"Hello, world!\")
*** Delete File: obsolete.txt
*** End Patch

Important:
- You must include a header with your intended action (Add/Delete/Update)
- You must prefix new lines with + even when creating a new file"
  :input-schema '((type . "object")
                  (properties . ((patchText . ((type . "string")
                                               (description . "The full patch text that describes all changes to be made")))))
                  (required . ["patchText"]))
  :function #'mcp-server-emacs-tools--apply-patch-handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . t)
                 (idempotentHint . :false)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-apply-patch)

;;; mcp-server-emacs-tools-apply-patch.el ends here
