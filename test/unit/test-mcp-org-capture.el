;;; test-mcp-org-capture.el --- Tests for org-capture -*- lexical-binding: t; -*-
(require 'ert)
(require 'test-helpers)
(require 'json)

(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "../../tools"
                                         (file-name-directory this-file)))))
  (when tools-dir (add-to-list 'load-path tools-dir)))

(require 'mcp-server-emacs-tools-org-capture)

(ert-deftest mcp-test-org-capture-direct-mode ()
  "Direct mode appends a heading to the target file."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let* ((json (mcp-server-emacs-tools-org-capture--handler
                  `((file . ,path)
                    (title . "New top-level task")
                    (body . "Body content.")
                    (todo_state . "TODO")
                    (tags . ["followup"]))))
           (result (let ((json-object-type 'alist)) (json-read-from-string json))))
      (should (alist-get 'id result))
      (with-temp-buffer
        (insert-file-contents path)
        (should (string-match-p "TODO New top-level task" (buffer-string)))
        (should (string-match-p ":followup:" (buffer-string)))
        (should (string-match-p "Body content\\." (buffer-string)))))))

(ert-deftest mcp-test-org-capture-direct-under-parent ()
  "Direct mode under an outline_path appends as child."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (mcp-server-emacs-tools-org-capture--handler
     `((file . ,path)
       (outline_path . ["Project Alpha"])
       (title . "Child task")))
    (with-temp-buffer
      (insert-file-contents path)
      (should (string-match-p "\\*\\* Child task" (buffer-string))))))

(ert-deftest mcp-test-org-capture-rejects-path-outside-root ()
  "Direct mode refuses paths outside allowed roots."
  (let* ((outside (make-temp-file "mcp-outside-" nil ".org"))
         (mcp-server-emacs-tools-org-allowed-roots (list "/nonexistent-root")))
    (unwind-protect
        (let* ((json (mcp-server-emacs-tools-org-capture--handler
                      `((file . ,outside) (title . "x"))))
               (result (let ((json-object-type 'alist)) (json-read-from-string json))))
          (should (alist-get 'error result)))
      (when (file-exists-p outside) (delete-file outside)))))

(ert-deftest mcp-test-org-capture-registered ()
  "Tool is registered."
  (should (mcp-server-tools-exists-p "org-capture")))

(ert-deftest mcp-test-org-capture-template-mode ()
  "Template mode uses `org-capture-templates' and creates a new entry."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let ((org-capture-templates
           `(("t" "Todo" entry (file ,path) "* TODO %?\n%i" :immediate-finish t))))
      (let* ((json (mcp-server-emacs-tools-org-capture--handler
                    '((template_key . "t")
                      (content . "Task captured via template"))))
             (result (let ((json-object-type 'alist)) (json-read-from-string json))))
        (should (alist-get 'id result))
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "TODO Task captured via template" (buffer-string))))))))

(ert-deftest mcp-test-org-capture-substitute-cursor-respects-escape ()
  "`%%?' (escaped literal) is not treated as the cursor marker.
Regression test for the %? regex."
  (let ((fn #'mcp-server-emacs-tools-org-capture--substitute-cursor))
    ;; Plain %? gets substituted.
    (should (equal (funcall fn "A %? B" "X") "A X B"))
    ;; Escaped %%? stays intact; first REAL %? after it gets substituted.
    (should (equal (funcall fn "escape %%? then %? end" "HERE")
                   "escape %%? then HERE end"))
    ;; Only-escaped template is returned unchanged.
    (should (equal (funcall fn "only %%? literal" "X")
                   "only %%? literal"))
    ;; Empty content leaves a gap but otherwise behaves the same.
    (should (equal (funcall fn "A %? B" "") "A  B"))))

;; ---------------------------------------------------------------------------
;; --preprocess-template-string tests
;; ---------------------------------------------------------------------------

(ert-deftest mcp-test-org-capture-preprocess-text-prompt-substituted ()
  "Named text prompt is replaced by the matching template_variables value."
  (let* ((result (mcp-server-emacs-tools-org-capture--preprocess-template-string
                  "* TODO %^{Title}\n%?" '((Title . "Buy milk"))))
         (processed (car result)))
    (should (string= processed "* TODO Buy milk\n%?"))))

(ert-deftest mcp-test-org-capture-preprocess-text-prompt-fallback-option ()
  "When key is absent, first pipe option is used as fallback."
  (let* ((result (mcp-server-emacs-tools-org-capture--preprocess-template-string
                  "* %^{Priority|HIGH|LOW}" '()))
         (processed (car result)))
    (should (string= processed "* HIGH"))))

(ert-deftest mcp-test-org-capture-preprocess-text-prompt-fallback-empty ()
  "When key absent and no options, empty string is substituted."
  (let* ((result (mcp-server-emacs-tools-org-capture--preprocess-template-string
                  "* %^{Title}" '()))
         (processed (car result)))
    (should (string= processed "* "))))

(ert-deftest mcp-test-org-capture-preprocess-tag-prompt-removed ()
  "Anonymous %^g is removed from the template string."
  (let* ((result (mcp-server-emacs-tools-org-capture--preprocess-template-string
                  "* TODO task%^g" '()))
         (processed (car result)))
    (should (string= processed "* TODO task"))))

(ert-deftest mcp-test-org-capture-preprocess-named-tag-prompt-removed ()
  "Named %^{TAG}g is removed from the template string."
  (let* ((result (mcp-server-emacs-tools-org-capture--preprocess-template-string
                  "* task%^{Tags}g" '()))
         (processed (car result)))
    (should (string= processed "* task"))))

(ert-deftest mcp-test-org-capture-preprocess-unnamed-date-removed ()
  "Unnamed %^t/%^T/%^u/%^U are removed from the template string."
  (let* ((result (mcp-server-emacs-tools-org-capture--preprocess-template-string
                  "SCHEDULED: %^t\nDEADLINE: %^T" '()))
         (processed (car result)))
    (should (string= processed "SCHEDULED: \nDEADLINE: "))))

(ert-deftest mcp-test-org-capture-preprocess-named-date-substituted ()
  "Named %^{NAME}t is replaced inline with the value from template_variables."
  (let* ((result (mcp-server-emacs-tools-org-capture--preprocess-template-string
                  "SCHEDULED: %^{sched}t" '((sched . "<2026-04-22 Wed>"))))
         (processed (car result)))
    (should (string= processed "SCHEDULED: <2026-04-22 Wed>"))))

(ert-deftest mcp-test-org-capture-preprocess-property-prompt-removed ()
  "Property prompt %^{NAME}p is removed from the string; value in post-props."
  (let* ((result (mcp-server-emacs-tools-org-capture--preprocess-template-string
                  "* task %^{effort}p" '((effort . "2h"))))
         (processed (car result))
         (post-props (cdr result)))
    (should (string= processed "* task "))
    (let ((prop (alist-get 'property post-props)))
      (should prop)
      (should (equal (car prop) "effort"))
      (should (equal (cdr prop) "2h")))))

(ert-deftest mcp-test-org-capture-preprocess-property-prompt-default ()
  "Property prompt with absent key uses empty string; still appears in post-props."
  (let* ((result (mcp-server-emacs-tools-org-capture--preprocess-template-string
                  "* %^{effort}p" '()))
         (post-props (cdr result)))
    (let ((prop (alist-get 'property post-props)))
      (should prop)
      (should (equal (car prop) "effort"))
      (should (equal (cdr prop) "")))))

(ert-deftest mcp-test-org-capture-preprocess-clipboard-substituted ()
  "Anonymous %^C is replaced by empty string when no var supplied."
  (let* ((result (mcp-server-emacs-tools-org-capture--preprocess-template-string
                  "see %^C here" '()))
         (processed (car result)))
    (should (string= processed "see  here"))))

(ert-deftest mcp-test-org-capture-preprocess-backref-resolved ()
  "%\\1 is replaced with the value of the first named text prompt."
  (let* ((result (mcp-server-emacs-tools-org-capture--preprocess-template-string
                  "* %^{Title} - see %\\1" '((Title . "Foo"))))
         (processed (car result)))
    (should (string= processed "* Foo - see Foo"))))

(ert-deftest mcp-test-org-capture-preprocess-backref-second ()
  "%\\2 resolves to the second named prompt's value."
  (let* ((result (mcp-server-emacs-tools-org-capture--preprocess-template-string
                  "* %^{Title} [%^{Tag}] copy: %\\1/%\\2"
                  '((Title . "A") (Tag . "B"))))
         (processed (car result)))
    (should (string= processed "* A [B] copy: A/B"))))

(ert-deftest mcp-test-org-capture-preprocess-escape-preserved ()
  "%%^{Title} escape is kept verbatim and the name is not substituted."
  (let* ((result (mcp-server-emacs-tools-org-capture--preprocess-template-string
                  "* %%^{Title} plain" '((Title . "X"))))
         (processed (car result)))
    (should (string= processed "* %%^{Title} plain"))))

(ert-deftest mcp-test-org-capture-preprocess-global-tag-removed ()
  "%^G (global tag prompt) is removed from string."
  (let* ((result (mcp-server-emacs-tools-org-capture--preprocess-template-string
                  "heading%^G" '()))
         (processed (car result)))
    (should (string= processed "heading"))))

(ert-deftest mcp-test-org-capture-inject-immediate-finish-when-absent ()
  ":immediate-finish t is added when option list lacks it."
  (let ((result (mcp-server-emacs-tools-org-capture--inject-immediate-finish
                 '(:prepend t))))
    (should (eq (plist-get result :immediate-finish) t))
    (should (eq (plist-get result :prepend) t))))

(ert-deftest mcp-test-org-capture-inject-immediate-finish-already-present ()
  "When :immediate-finish is already present, it is not duplicated."
  (let* ((opts '(:immediate-finish t :prepend t))
         (result (mcp-server-emacs-tools-org-capture--inject-immediate-finish opts)))
    ;; Count occurrences of :immediate-finish key in the plist
    (should (= 1 (cl-count :immediate-finish result)))))

(ert-deftest mcp-test-org-capture-inject-immediate-finish-empty-opts ()
  "Empty option list gains :immediate-finish t."
  (let ((result (mcp-server-emacs-tools-org-capture--inject-immediate-finish nil)))
    (should (eq (plist-get result :immediate-finish) t))))

(ert-deftest mcp-test-org-capture-template-text-prompt-from-vars ()
  "Template mode substitutes %^{Title} from template_variables."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let ((org-capture-templates
           `(("v" "Vars" entry (file ,path) "* TODO %^{Title}\n%?"))))
      (let* ((json (mcp-server-emacs-tools-org-capture--handler
                    '((template_key . "v")
                      (template_variables . ((Title . "Buy groceries"))))))
             (result (let ((json-object-type 'alist)) (json-read-from-string json))))
        (should-not (alist-get 'error result))
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "TODO Buy groceries" (buffer-string))))))))

(ert-deftest mcp-test-org-capture-template-injects-immediate-finish ()
  "Template mode does not block even when :immediate-finish t is absent."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let ((org-capture-templates
           `(("n" "No-finish" entry (file ,path) "* TODO no-finish-test"))))
      (let* ((json (mcp-server-emacs-tools-org-capture--handler
                    '((template_key . "n"))))
             (result (let ((json-object-type 'alist)) (json-read-from-string json))))
        (should-not (alist-get 'error result))
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "no-finish-test" (buffer-string))))))))

(ert-deftest mcp-test-org-capture-template-tags-applied-post-capture ()
  "tags arg is applied post-capture even when %^g is in the template."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let ((org-capture-templates
           `(("g" "Tagged" entry (file ,path) "* TODO task%^g"))))
      (let* ((json (mcp-server-emacs-tools-org-capture--handler
                    `((template_key . "g")
                      (tags . ["work" "urgent"]))))
             (result (let ((json-object-type 'alist)) (json-read-from-string json))))
        (should-not (alist-get 'error result))
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p ":work:" (buffer-string)))
          (should (string-match-p ":urgent:" (buffer-string))))))))

(ert-deftest mcp-test-org-capture-template-scheduled-applied-post-capture ()
  "scheduled arg is applied post-capture when template has %^t."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let ((org-capture-templates
           `(("s" "Sched" entry (file ,path) "* TODO sched-test%^t"))))
      (let* ((json (mcp-server-emacs-tools-org-capture--handler
                    `((template_key . "s")
                      (scheduled . "<2026-04-22 Wed>"))))
             (result (let ((json-object-type 'alist)) (json-read-from-string json))))
        (should-not (alist-get 'error result))
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "SCHEDULED:" (buffer-string))))))))

(ert-deftest mcp-test-org-capture-template-property-from-prompt ()
  "Property from %^{NAME}p is set post-capture via org-set-property."
  (mcp-test-with-org-fixture "sample-notes.org" path
    (let ((org-capture-templates
           `(("p" "Prop" entry (file ,path) "* TODO prop-test %^{effort}p"))))
      (let* ((json (mcp-server-emacs-tools-org-capture--handler
                    '((template_key . "p")
                      (template_variables . ((effort . "2h"))))))
             (result (let ((json-object-type 'alist)) (json-read-from-string json))))
        (should-not (alist-get 'error result))
        (with-temp-buffer
          (insert-file-contents path)
          (should (string-match-p "effort.*2h\\|2h.*effort" (buffer-string))))))))

(provide 'test-mcp-org-capture)
;;; test-mcp-org-capture.el ends here
