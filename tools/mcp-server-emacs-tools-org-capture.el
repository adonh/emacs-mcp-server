;;; mcp-server-emacs-tools-org-capture.el --- org-capture MCP tool -*- lexical-binding: t; -*-

;;; Commentary:

;; Create a new org entry.  Two modes: template (preferred) and direct.

;;; Code:

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-org-common)
(require 'org)
(require 'org-capture)
(require 'org-id)
(require 'json)
(require 'seq)

(defconst mcp-server-emacs-tools-org-capture--marker-regexp
  (concat "\\(%%\\)"
          "\\|%\\^\\(?:"
          "{\\([^|}]*\\)\\(?:|\\([^}]*\\)\\)?}"
          "\\([gGtTuUCLp]?\\)"
          "\\|\\([gGtTuUCLp]\\)\\)")
  "Regexp matching %^ interactive markers and %% escapes in org-capture templates.")

(defun mcp-server-emacs-tools-org-capture--preprocess-template-string (tmpl vars)
  "Pre-substitute all %^ interactive markers in TMPL.
VARS is an alist of (SYMBOL . value) pairs; SYMBOL names match the NAME
in %^{NAME} markers (compared via `intern').
Returns (PROCESSED-STRING . POST-PROPS) where POST-PROPS is an alist of
\((property NAME . VALUE) ...) entries for post-capture application.

%^{NAME} text prompts are replaced inline with VARS value, first pipe
option, or \"\".  Named date prompts (%^{NAME}t etc.) are replaced with
VARS value or \"\".  Unnamed date prompts (%^t etc.) and tag prompts
(%^g/%^G) are removed from the string.  %^{NAME}p property prompts are
removed and recorded in POST-PROPS.  %^C/%^L clipboard prompts are
replaced with VARS value or \"\".  %\\N back-references are resolved to
the Nth collected named-prompt value (1-based).  %%^ escapes are kept
verbatim."
  (let ((result "")
        (start 0)
        (named-values '())
        (post-props '()))
    (while (string-match mcp-server-emacs-tools-org-capture--marker-regexp tmpl start)
      (setq result (concat result (substring tmpl start (match-beginning 0))))
      (cond
       ;; %% escape: keep verbatim
       ((match-beginning 1)
        (setq result (concat result "%%"))
        (setq start (match-end 0)))
       (t
        ;; Save match-end before any internal string-match calls clobber it.
        (let* ((match-end-pos (match-end 0))
               (name    (and (match-beginning 2)
                             (match-string-no-properties 2 tmpl)))
               (opts    (and (match-beginning 3)
                             (match-string-no-properties 3 tmpl)))
               (suf4    (and (match-beginning 4)
                             (let ((s (match-string-no-properties 4 tmpl)))
                               (when (> (length s) 0) s))))
               (suf5    (and (match-beginning 5)
                             (match-string-no-properties 5 tmpl)))
               (suffix  (or suf4 suf5 ""))
               (raw-val (let ((v (and name
                                       (alist-get (intern name) vars 'mcp-server-emacs-tools-org-capture--absent))))
                           (if (and (not (eq v 'mcp-server-emacs-tools-org-capture--absent))
                                    v
                                    (not (eq v :null)))
                               (format "%s" v)
                             (or (and opts (car (split-string opts "|" t)))
                                 "")))))
          (pcase suffix
            ;; Text prompt (no suffix) - substitute inline
            (""
             (push raw-val named-values)
             (setq result (concat result raw-val)))
            ;; Tag prompts - remove from string
            ((or "g" "G") nil)
            ;; Named date prompts - substitute inline; unnamed - remove
            ((or "t" "T" "u" "U")
             (when name
               (setq result (concat result raw-val))))
            ;; Property prompt - remove; record for post-capture
            ("p"
             (when name
               (push (cons 'property (cons name raw-val)) post-props)))
            ;; Clipboard / link - substitute inline
            ((or "C" "L")
             (setq result (concat result raw-val))))
          (setq start match-end-pos)))))
    ;; Append remaining text
    (setq result (concat result (substring tmpl start)))
    ;; Resolve %\N back-references (1-based, into named-values collected in order)
    (let ((refs (nreverse named-values))
          (ref-result "")
          (ref-start 0))
      (while (string-match "%\\\\\\([0-9]+\\)" result ref-start)
        (let* ((match-start (match-beginning 0))
               (n (string-to-number (match-string 1 result)))
               (escaped (and (> match-start 0)
                             (= (aref result (1- match-start)) ?%))))
          (if escaped
              (progn
                (setq ref-result (concat ref-result
                                         (substring result ref-start (match-end 0))))
                (setq ref-start (match-end 0)))
            (let ((val (or (nth (1- n) refs) "")))
              (setq ref-result (concat ref-result
                                       (substring result ref-start match-start)
                                       val))
              (setq ref-start (match-end 0))))))
      (setq result (concat ref-result (substring result ref-start))))
    (cons result (nreverse post-props))))

(defun mcp-server-emacs-tools-org-capture--substitute-cursor (template content)
  "Return TEMPLATE with the first unescaped `%?' replaced by CONTENT.
MCP calls are non-interactive, so the `%?' cursor marker is pre-filled
with CONTENT before org-capture processes the template.  When TEMPLATE
contains no `%?', TEMPLATE is returned unchanged.  Respects org-capture
`%%' escaping: a literal `%?' in the output (written `%%?' in the
template) is not treated as the cursor marker."
  (if (not (and (stringp template) (stringp content)))
      template
    (let ((start 0) done result)
      (while (and (not done)
                  (string-match "\\(%%\\)\\|\\(%\\?\\)" template start))
        (cond
         ;; %% -> keep both characters and continue past them.
         ((match-beginning 1)
          (setq start (match-end 0)))
         ;; %? -> splice content in place of the match and stop.
         (t
          (setq result (concat (substring template 0 (match-beginning 2))
                               content
                               (substring template (match-end 2))))
          (setq done t))))
      (or result template))))

(defun mcp-server-emacs-tools-org-capture--inject-immediate-finish (options)
  "Return OPTIONS plist with `:immediate-finish t' added when absent."
  (if (plist-member options :immediate-finish)
      options
    (append options '(:immediate-finish t))))

(defun mcp-server-emacs-tools-org-capture--template-mode (args)
  "Run org-capture with a template from ARGS.
Always finalizes synchronously; `immediate_finish' arg is accepted but ignored
because MCP calls are non-interactive.  The `content' arg is placed at the
template's `%?' cursor marker (when present) and also made available as `%i'."
  (let* ((key (alist-get 'template_key args))
         (content (alist-get 'content args))
         (entry (and key (assoc key org-capture-templates)))
         (modified-entry
          (and entry content (stringp (nth 4 entry))
               (append (seq-take entry 4)
                       (list (mcp-server-emacs-tools-org-capture--substitute-cursor
                              (nth 4 entry) content))
                       (nthcdr 5 entry))))
         (modified-templates
          (if modified-entry
              (cons modified-entry
                    (seq-remove (lambda (e) (equal (car-safe e) key))
                                org-capture-templates))
            org-capture-templates))
         (org-capture-templates modified-templates)
         (org-capture-initial (or content ""))
         (org-capture-templates-contexts nil))
    (save-window-excursion
      (org-capture nil key)
      (when (buffer-live-p (get-buffer "*Capture*"))
        (with-current-buffer "*Capture*" (org-capture-finalize))))
    (when (and (boundp 'org-capture-last-stored-marker)
               (markerp org-capture-last-stored-marker))
      (let ((marker org-capture-last-stored-marker))
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (let* ((id (mcp-server-emacs-tools-org-common--promote-to-id (point-marker)))
                   (file (buffer-file-name))
                   (olp (mcp-server-emacs-tools-org-common--compute-outline-path)))
              `((id . ,id) (file . ,file) (outline_path . ,(vconcat olp))))))))))

(defun mcp-server-emacs-tools-org-capture--direct-mode (args)
  "Create an entry directly from ARGS without a template."
  (let* ((file (or (alist-get 'file args) (error "Direct mode requires `file'")))
         (_ (mcp-server-emacs-tools-org-common--validate-path file))
         (outline-path (alist-get 'outline_path args))
         (title (or (alist-get 'title args) (error "`title' is required")))
         (body (alist-get 'body args))
         (todo-state (alist-get 'todo_state args))
         (priority (alist-get 'priority args))
         (tags (alist-get 'tags args))
         (properties (alist-get 'properties args))
         (scheduled (alist-get 'scheduled args))
         (deadline (alist-get 'deadline args))
         (buf (find-file-noselect file)))
    (with-current-buffer buf
      (org-with-wide-buffer
       (let (insert-marker level)
         (if outline-path
             (let ((parent-marker (mcp-server-emacs-tools-org-common--resolve-node
                                   `((file . ,file)
                                     (outline_path . ,outline-path)))))
               (goto-char parent-marker)
               (setq level (1+ (org-outline-level)))
               (org-end-of-subtree t t)
               (unless (bolp) (insert "\n"))
               (setq insert-marker (point-marker)))
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (setq insert-marker (point-marker))
           (setq level 1))
         (goto-char insert-marker)
         (insert (concat (make-string level ?*) " "
                         (when todo-state (concat todo-state " "))
                         title "\n"))
         (forward-line -1)
         (when tags (org-set-tags (append tags nil)))
         (when priority (org-priority (string-to-char priority)))
         (when properties
           (dolist (p properties)
             (org-set-property (format "%s" (car p)) (format "%s" (cdr p)))))
         (when scheduled (org-schedule nil scheduled))
         (when deadline (org-deadline nil deadline))
         (let ((id (mcp-server-emacs-tools-org-common--promote-to-id (point-marker))))
           (when body
             (org-end-of-meta-data t)
             (unless (bolp) (insert "\n"))
             (insert body)
             (unless (bolp) (insert "\n")))
           (when mcp-server-emacs-tools-org-auto-save (save-buffer))
           `((id . ,id)
             (file . ,file)
             (outline_path . ,(vconcat (mcp-server-emacs-tools-org-common--compute-outline-path))))))))))

(defun mcp-server-emacs-tools-org-capture--handler (args)
  "Handle org-capture tool call with ARGS."
  (condition-case err
      (let ((result
             (cond
              ((alist-get 'template_key args)
               (mcp-server-emacs-tools-org-capture--template-mode args))
              (t
               (mcp-server-emacs-tools-org-capture--direct-mode args)))))
        (json-encode result))
    (error (json-encode `((error . ,(error-message-string err)))))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "org-capture"
  :title "Org Capture"
  :description "Create a new org entry.  Prefer `template_key' with an existing user template (see org-list-templates) because it respects user-configured targets and formatting.  Use direct mode (`file' + `title' + optional `outline_path'/`body'/`todo_state'/`tags') only when no template fits.  Before setting tags, check org-list-tags to avoid near-duplicates."
  :input-schema '((type . "object")
                  (properties . ((template_key . ((type . "string")))
                                 (content . ((type . "string")))
                                 (file . ((type . "string")))
                                 (outline_path . ((type . "array")
                                                  (items . ((type . "string")))))
                                 (title . ((type . "string")))
                                 (body . ((type . "string")))
                                 (todo_state . ((type . "string")))
                                 (priority . ((type . "string")))
                                 (tags . ((type . "array")
                                          (items . ((type . "string")))))
                                 (properties . ((type . "object")))
                                 (scheduled . ((type . "string")))
                                 (deadline . ((type . "string")))))
                  (required . []))
  :function #'mcp-server-emacs-tools-org-capture--handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . t)
                 (idempotentHint . :false)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-org-capture)

;;; mcp-server-emacs-tools-org-capture.el ends here
