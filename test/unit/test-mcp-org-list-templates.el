;;; test-mcp-org-list-templates.el --- Tests for org-list-templates -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-list-templates)

(ert-deftest mcp-test-org-list-templates-capture ()
  "list-templates returns configured capture templates."
  (let ((org-capture-templates
         '(("t" "Todo" entry (file "/tmp/inbox.org") "* TODO %?" :immediate-finish t)
           ("j" "Journal" entry (file+datetree "/tmp/journal.org") "* %?"))))
    (let* ((json (mcp-server-emacs-tools-org-list-templates--handler
                  '((type . "capture"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (templates (alist-get 'templates result)))
      (should (= (length templates) 2))
      (let ((first (aref templates 0)))
        (should (equal (alist-get 'key first) "t"))
        (should (equal (alist-get 'description first) "Todo"))))))

(ert-deftest mcp-test-org-list-templates-roam-without-roam ()
  "list-templates roam type returns error when roam is absent.
The handler uses `(require 'org-roam nil t)`; mock that to return nil
for `org-roam' regardless of whether the package is installed."
  (cl-letf* ((orig-require (symbol-function 'require))
             ((symbol-function 'require)
              (lambda (feat &optional filename noerror)
                (if (eq feat 'org-roam) nil
                  (funcall orig-require feat filename noerror)))))
    (let* ((json (mcp-server-emacs-tools-org-list-templates--handler
                  '((type . "roam-capture"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (alist-get 'error result)))))

(ert-deftest mcp-test-org-list-templates-registered ()
  "Tool is registered."
  (should (mcp-server-tools-exists-p "org-list-templates")))

(ert-deftest mcp-test-org-list-templates-file-headline ()
  "Serializes a file+headline target correctly."
  (let ((org-capture-templates
         '(("h" "Heading" entry (file+headline "/tmp/notes.org" "Inbox") "* %?"))))
    (let* ((json (mcp-server-emacs-tools-org-list-templates--handler
                  '((type . "capture"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (tmpl (aref (alist-get 'templates result) 0)))
      (should (equal (alist-get 'target_type tmpl) "file+headline"))
      (should (equal (alist-get 'target_file tmpl) "/tmp/notes.org"))
      (should (equal (alist-get 'target_heading tmpl) "Inbox")))))

(ert-deftest mcp-test-org-list-templates-file-olp ()
  "Serializes a file+olp target (outline path as list)."
  (let ((org-capture-templates
         '(("o" "OLP" entry (file+olp "/tmp/notes.org" "Projects" "Active") "* %?"))))
    (let* ((json (mcp-server-emacs-tools-org-list-templates--handler
                  '((type . "capture"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (tmpl (aref (alist-get 'templates result) 0)))
      (should (equal (alist-get 'target_type tmpl) "file+olp"))
      (should (equal (alist-get 'target_file tmpl) "/tmp/notes.org")))))

(ert-deftest mcp-test-org-list-templates-file-datetree ()
  "Serializes a file+datetree target."
  (let ((org-capture-templates
         '(("j" "Journal" entry (file+datetree "/tmp/journal.org") "* %?"))))
    (let* ((json (mcp-server-emacs-tools-org-list-templates--handler
                  '((type . "capture"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json)))
           (tmpl (aref (alist-get 'templates result) 0)))
      (should (equal (alist-get 'target_type tmpl) "file+datetree"))
      (should (equal (alist-get 'target_file tmpl) "/tmp/journal.org")))))

(ert-deftest mcp-test-org-list-templates-empty ()
  "When no templates configured, returns an empty templates array."
  (let ((org-capture-templates nil))
    (let* ((json (mcp-server-emacs-tools-org-list-templates--handler
                  '((type . "capture"))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (vectorp (alist-get 'templates result)))
      (should (= (length (alist-get 'templates result)) 0)))))

(ert-deftest mcp-test-org-list-templates-extract-prompts-text ()
  "Named text prompt returns type=text with name and empty completions."
  (let ((result (mcp-server-emacs-tools-org-list-templates--extract-prompts
                 "* TODO %^{Title}\n%?")))
    (should (= (length result) 1))
    (let ((p (car result)))
      (should (equal (alist-get 'type p) "text"))
      (should (equal (alist-get 'name p) "Title"))
      (should (equal (alist-get 'completions p) [])))))

(ert-deftest mcp-test-org-list-templates-extract-prompts-completions ()
  "Text prompt with pipe options returns completions vector."
  (let ((result (mcp-server-emacs-tools-org-list-templates--extract-prompts
                 "* %^{Effort|1h|2h|4h}")))
    (should (= (length result) 1))
    (let ((p (car result)))
      (should (equal (alist-get 'name p) "Effort"))
      (should (equal (alist-get 'completions p) ["1h" "2h" "4h"])))))

(ert-deftest mcp-test-org-list-templates-extract-prompts-tags-local ()
  "%^g returns type=tags_local with no name."
  (let ((result (mcp-server-emacs-tools-org-list-templates--extract-prompts
                 "* heading%^g")))
    (should (= (length result) 1))
    (let ((p (car result)))
      (should (equal (alist-get 'type p) "tags_local"))
      (should (null (alist-get 'name p))))))

(ert-deftest mcp-test-org-list-templates-extract-prompts-tags-global ()
  "%^G returns type=tags_global."
  (let ((result (mcp-server-emacs-tools-org-list-templates--extract-prompts "%^G")))
    (should (= (length result) 1))
    (should (equal (alist-get 'type (car result)) "tags_global"))))

(ert-deftest mcp-test-org-list-templates-extract-prompts-date-types ()
  "%^t and %^T both return type=date; %^u and %^U return type=date_inactive."
  (let ((result (mcp-server-emacs-tools-org-list-templates--extract-prompts
                 "%^t %^T %^u %^U")))
    (should (= (length result) 4))
    (should (equal (alist-get 'type (nth 0 result)) "date"))
    (should (equal (alist-get 'type (nth 1 result)) "date"))
    (should (equal (alist-get 'type (nth 2 result)) "date_inactive"))
    (should (equal (alist-get 'type (nth 3 result)) "date_inactive"))))

(ert-deftest mcp-test-org-list-templates-extract-prompts-property ()
  "%^{effort}p returns type=property with name=effort."
  (let ((result (mcp-server-emacs-tools-org-list-templates--extract-prompts
                 "%^{effort}p")))
    (should (= (length result) 1))
    (let ((p (car result)))
      (should (equal (alist-get 'type p) "property"))
      (should (equal (alist-get 'name p) "effort")))))

(ert-deftest mcp-test-org-list-templates-extract-prompts-clipboard-link ()
  "%^C and %^L return type=clipboard and type=link."
  (let ((result (mcp-server-emacs-tools-org-list-templates--extract-prompts
                 "see %^C also %^L")))
    (should (= (length result) 2))
    (should (equal (alist-get 'type (nth 0 result)) "clipboard"))
    (should (equal (alist-get 'type (nth 1 result)) "link"))))

(ert-deftest mcp-test-org-list-templates-extract-prompts-named-tag ()
  "%^{TAG}g has name and type=tags_local."
  (let ((result (mcp-server-emacs-tools-org-list-templates--extract-prompts
                 "* heading%^{Tags}g")))
    (should (= (length result) 1))
    (let ((p (car result)))
      (should (equal (alist-get 'type p) "tags_local"))
      (should (equal (alist-get 'name p) "Tags")))))

(ert-deftest mcp-test-org-list-templates-extract-prompts-named-date ()
  "%^{Due}t has name and type=date."
  (let ((result (mcp-server-emacs-tools-org-list-templates--extract-prompts
                 "SCHEDULED: %^{Due}t")))
    (should (= (length result) 1))
    (let ((p (car result)))
      (should (equal (alist-get 'type p) "date"))
      (should (equal (alist-get 'name p) "Due")))))

(ert-deftest mcp-test-org-list-templates-extract-prompts-escape ()
  "%%^{Title} is an escape - not extracted as a prompt."
  (let ((result (mcp-server-emacs-tools-org-list-templates--extract-prompts
                 "* %%^{Title} plain text")))
    (should (null result))))

(ert-deftest mcp-test-org-list-templates-extract-prompts-non-string ()
  "Non-string template (e.g. function) returns nil without error."
  (should (null (mcp-server-emacs-tools-org-list-templates--extract-prompts nil)))
  (should (null (mcp-server-emacs-tools-org-list-templates--extract-prompts 42))))

(ert-deftest mcp-test-org-list-templates-extract-prompts-multiple ()
  "Multiple prompts returned in document order."
  (let ((result (mcp-server-emacs-tools-org-list-templates--extract-prompts
                 "* %^{Title}%^g\nSCHEDULED: %^t\n%^{effort}p")))
    (should (= (length result) 4))
    (should (equal (alist-get 'type (nth 0 result)) "text"))
    (should (equal (alist-get 'type (nth 1 result)) "tags_local"))
    (should (equal (alist-get 'type (nth 2 result)) "date"))
    (should (equal (alist-get 'type (nth 3 result)) "property"))))

(provide 'test-mcp-org-list-templates)
;;; test-mcp-org-list-templates.el ends here
