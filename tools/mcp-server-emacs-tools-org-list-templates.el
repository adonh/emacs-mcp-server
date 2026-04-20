;;; mcp-server-emacs-tools-org-list-templates.el --- org-list-templates MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Return the user's configured capture templates so LLM clients can
;; choose an appropriate template_key for org-capture or org-roam-capture.

;;; Code:

(require 'mcp-server-tools)
(require 'org-capture)
(require 'json)

(defconst mcp-server-emacs-tools-org-list-templates--marker-regexp
  (concat "\\(%%\\)"
          "\\|%\\^\\(?:"
          "{\\([^|}]*\\)\\(?:|\\([^}]*\\)\\)?}"
          "\\([gGtTuUCLp]?\\)"
          "\\|\\([gGtTuUCLp]\\)\\)")
  "Regexp matching %^ interactive markers and %% escapes in org-capture templates.")

(defun mcp-server-emacs-tools-org-list-templates--extract-prompts (template-string)
  "Return a list of prompt descriptor alists from TEMPLATE-STRING.
Each descriptor has at minimum a `type' key (string).  Named prompts
also carry `name'.  Text prompts carry `completions' (a vector,
possibly empty).  Returns nil for non-string inputs."
  (when (stringp template-string)
    (let ((prompts '())
          (start 0))
      (while (string-match
              mcp-server-emacs-tools-org-list-templates--marker-regexp
              template-string start)
        (cond
         ;; %% escape: skip
         ((match-beginning 1)
          (setq start (match-end 0)))
         (t
          (let* ((name    (and (match-beginning 2)
                               (match-string-no-properties 2 template-string)))
                 (opts    (and (match-beginning 3)
                               (match-string-no-properties 3 template-string)))
                 (suf4    (and (match-beginning 4)
                               (let ((s (match-string-no-properties 4 template-string)))
                                 (when (> (length s) 0) s))))
                 (suf5    (and (match-beginning 5)
                               (match-string-no-properties 5 template-string)))
                 (suffix  (or suf4 suf5 ""))
                 (type    (pcase suffix
                            ("g"          "tags_local")
                            ("G"          "tags_global")
                            ((or "t" "T") "date")
                            ((or "u" "U") "date_inactive")
                            ("p"          "property")
                            ("C"          "clipboard")
                            ("L"          "link")
                            (_            "text")))
                 (completions
                  (when (string= type "text")
                    (if opts (vconcat (split-string opts "|" t)) [])))
                 (descriptor
                  (append
                   `((type . ,type))
                   (when name `((name . ,name)))
                   (when (string= type "text") `((completions . ,completions))))))
            (push descriptor prompts)
            (setq start (match-end 0))))))
      (nreverse prompts))))

(defun mcp-server-emacs-tools-org-list-templates--template-to-alist (tmpl)
  "Convert TMPL (an entry from org-capture-templates) to an alist.
For `file+olp' and `file+olp+datetree' targets, the full outline path
is returned as a vector under `target_outline_path'; `target_heading'
is kept as the first path component for backward compatibility."
  (let* ((key (nth 0 tmpl))
         (description (nth 1 tmpl))
         (kind (nth 2 tmpl))
         (target (nth 3 tmpl))
         (target-type (when (listp target) (car target)))
         (target-file (when (and (listp target) (>= (length target) 2))
                        (let ((f (nth 1 target)))
                          (if (stringp f) f (format "%S" f)))))
         (target-olp
          (pcase target-type
            ((or 'file+headline 'file+olp 'file+olp+datetree)
             (let ((raw (nthcdr 2 target)))
               (mapcar (lambda (x) (if (stringp x) x (format "%S" x))) raw)))
            (_ nil)))
         (target-heading (car target-olp)))
    (let* ((raw-tmpl (nth 4 tmpl))
           (prompts  (mcp-server-emacs-tools-org-list-templates--extract-prompts
                      (when (stringp raw-tmpl) raw-tmpl))))
      `((key . ,key)
        (description . ,description)
        (kind . ,(symbol-name (or kind 'entry)))
        (target_type . ,(when target-type (symbol-name target-type)))
        (target_file . ,target-file)
        (target_heading . ,target-heading)
        (target_outline_path . ,(vconcat target-olp))
        (prompts . ,(vconcat prompts))))))

(defun mcp-server-emacs-tools-org-list-templates--handler (args)
  "Handle org-list-templates tool call with ARGS."
  (condition-case err
      (let* ((type (or (alist-get 'type args) "capture")))
        (pcase type
          ("capture"
           (let ((templates (mapcar #'mcp-server-emacs-tools-org-list-templates--template-to-alist
                                    (or org-capture-templates '()))))
             (json-encode `((templates . ,(vconcat templates))))))
          ("roam-capture"
           (if (require 'org-roam nil t)
               (let ((templates (mapcar #'mcp-server-emacs-tools-org-list-templates--template-to-alist
                                        (or (and (boundp 'org-roam-capture-templates)
                                                 org-roam-capture-templates)
                                            '()))))
                 (json-encode `((templates . ,(vconcat templates)))))
             (json-encode `((error . "org-roam not installed")))))
          (_ (error "Unknown type: %s" type))))
    (error (json-encode `((error . ,(error-message-string err)))))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "org-list-templates"
  :title "List Capture Templates"
  :description "Return the user's configured capture templates so you can pick the right template_key when calling org-capture or org-roam-capture."
  :input-schema '((type . "object")
                  (properties . ((type . ((type . "string")
                                          (enum . ("capture" "roam-capture"))))))
                  (required . []))
  :function #'mcp-server-emacs-tools-org-list-templates--handler
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false)
                 (idempotentHint . t)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-org-list-templates)

;;; mcp-server-emacs-tools-org-list-templates.el ends here
