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
          "{\\([^}]*\\)}\\([gGtTuUCLp]?\\)"
          "\\|\\([gGtTuUCLp]\\)\\)")
  "Regexp matching %^ interactive markers and %% escapes in org-capture templates.
Group 1: %% escape.  Group 2: full {content} of a braced prompt.
Group 3: optional suffix letter after a braced prompt.
Group 4: suffix letter of a bare %^G-style prompt.")

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
          (let* (;; Group 2: full text between { and } (e.g. "name|opt1|opt2")
                 (content (and (match-beginning 2)
                               (match-string-no-properties 2 template-string)))
                 ;; Group 3: optional suffix after } (e.g. "p" in %^{name}p)
                 (suf3    (and (match-beginning 3)
                               (let ((s (match-string-no-properties 3 template-string)))
                                 (when (> (length s) 0) s))))
                 ;; Group 4: suffix for bare %^G-style prompts
                 (suf4    (and (match-beginning 4)
                               (match-string-no-properties 4 template-string)))
                 (suffix  (or suf3 suf4 ""))
                 (type    (pcase suffix
                            ("g"          "tags_local")
                            ("G"          "tags_global")
                            ((or "t" "T") "date")
                            ((or "u" "U") "date_inactive")
                            ("p"          "property")
                            ("C"          "clipboard")
                            ("L"          "link")
                            (_            "text")))
                 ;; Split content by | to get name and completion options
                 (parts   (when (and content (> (length content) 0))
                            (split-string content "|" t)))
                 (name    (car parts))
                 (completions
                  (when (string= type "text")
                    (if (cdr parts) (vconcat (cdr parts)) [])))
                 (descriptor
                  (append
                   `((type . ,type))
                   (when name `((name . ,name)))
                   (when (string= type "text") `((completions . ,completions))))))
            (push descriptor prompts)
            (setq start (match-end 0))))))
      (nreverse prompts))))

(defun mcp-server-emacs-tools-org-list-templates--safe-string (val)
  "Return VAL as a plain string, or nil if VAL cannot be safely stringified.
Strips text properties from strings.  Converts symbols via `symbol-name'.
Returns nil for any other type to avoid calling `format' on lambdas or
closures, which can overflow the stack when printing large environments."
  (cond ((stringp val) (substring-no-properties val))
        ((symbolp val) (symbol-name val))
        (t nil)))

(defun mcp-server-emacs-tools-org-list-templates--template-to-alist (tmpl)
  "Convert TMPL (an entry from org-capture-templates) to an alist.
For `file+olp' and `file+olp+datetree' targets, the full outline path
is returned as a vector under `target_outline_path'; `target_heading'
is kept as the first path component for backward compatibility.
Returns nil when the entry cannot be safely converted (e.g. group headers
or templates with dynamic targets that cannot be serialised)."
  (condition-case nil
      (let* ((key         (nth 0 tmpl))
             (description (mcp-server-emacs-tools-org-list-templates--safe-string
                           (nth 1 tmpl)))
             (kind        (nth 2 tmpl))
             (target      (nth 3 tmpl))
             (target-type (when (and (consp target) (symbolp (car target)))
                            (car target)))
             (target-file (when (consp target)
                            (mcp-server-emacs-tools-org-list-templates--safe-string
                             (nth 1 target))))
             (target-olp
              (pcase target-type
                ((or 'file+headline 'file+olp 'file+olp+datetree)
                 (delq nil
                       (mapcar #'mcp-server-emacs-tools-org-list-templates--safe-string
                               (nthcdr 2 target))))
                (_ nil)))
             (target-heading (car target-olp)))
        (let* ((raw-tmpl (nth 4 tmpl))
               (prompts  (mcp-server-emacs-tools-org-list-templates--extract-prompts
                          (when (stringp raw-tmpl) raw-tmpl))))
          `((key . ,key)
            (description . ,description)
            (kind . ,(if (symbolp kind) (symbol-name kind) "entry"))
            (target_type . ,(when target-type (symbol-name target-type)))
            (target_file . ,target-file)
            (target_heading . ,target-heading)
            (target_outline_path . ,(vconcat target-olp))
            (prompts . ,(vconcat prompts)))))
    (error nil)))

(defun mcp-server-emacs-tools-org-list-templates--handler (args)
  "Handle org-list-templates tool call with ARGS."
  (condition-case err
      (let* ((type (or (alist-get 'type args) "capture")))
        (pcase type
          ("capture"
           (let ((templates (delq nil
                                  (mapcar #'mcp-server-emacs-tools-org-list-templates--template-to-alist
                                          (or org-capture-templates '())))))
             (json-encode `((templates . ,(vconcat templates))))))
          ("roam-capture"
           (if (require 'org-roam nil t)
               (let ((templates (delq nil
                                      (mapcar #'mcp-server-emacs-tools-org-list-templates--template-to-alist
                                              (or (and (boundp 'org-roam-capture-templates)
                                                       org-roam-capture-templates)
                                                  '())))))
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
