;;; test-mcp-opencode-tools-integration.el --- Integration Tests for MCP Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Integration tests that exercise multiple tools together,
;; simulating real AI agent workflows like:
;; - read then edit
;; - write then grep
;; - glob then read multiple files
;; - write/edit then apply_patch
;; - todo management lifecycle

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load path setup
(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root (expand-file-name "../../" this-dir))
       (tools-dir (expand-file-name "tools" root)))
  (add-to-list 'load-path root)
  (add-to-list 'load-path tools-dir))

(require 'mcp-server-tools)
(require 'mcp-server-emacs-tools-read)
(require 'mcp-server-emacs-tools-write)
(require 'mcp-server-emacs-tools-edit)
(require 'mcp-server-emacs-tools-grep)
(require 'mcp-server-emacs-tools-glob)
(require 'mcp-server-emacs-tools-ls)
(require 'mcp-server-emacs-tools-bash)
(require 'mcp-server-emacs-tools-todo)
(require 'mcp-server-emacs-tools-multiedit)
(require 'mcp-server-emacs-tools-apply-patch)
(require 'mcp-server-emacs-tools-batch)
(require 'mcp-server-emacs-tools-task)
(require 'mcp-server-emacs-tools-skill)

;;; Helpers

(defmacro mcp-test-integration-with-dir (var &rest body)
  "Create temp dir bound to VAR and execute BODY, then cleanup."
  (declare (indent 1))
  `(let ((,var (make-temp-file "mcp-integration-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var) (delete-directory ,var t)))))

;;; ============================================================
;;; READ → EDIT WORKFLOW
;;; ============================================================

(ert-deftest mcp-integration-read-then-edit ()
  "Integration: read a file, verify content, then edit it."
  (mcp-test-integration-with-dir dir
    (let ((filepath (expand-file-name "source.py" dir)))
      (write-region "def greet():\n    return 'Hello'\n" nil filepath nil 'silent)
      ;; Step 1: Read
      (let ((read-result (mcp-server-emacs-tools--read-handler
                          `((filePath . ,filepath)))))
        (should (string-match-p "def greet" read-result))
        (should (string-match-p "Hello" read-result)))
      ;; Step 2: Edit
      (let ((edit-result (mcp-server-emacs-tools--edit-handler
                          `((filePath . ,filepath)
                            (oldString . "Hello")
                            (newString . "Goodbye")))))
        (should (string-match-p "applied successfully" edit-result)))
      ;; Step 3: Verify
      (let ((final (with-temp-buffer
                     (insert-file-contents filepath)
                     (buffer-string))))
        (should (string-match-p "Goodbye" final))
        (should-not (string-match-p "'Hello'" final))))))

;;; ============================================================
;;; WRITE → GREP WORKFLOW
;;; ============================================================

(ert-deftest mcp-integration-write-then-grep ()
  "Integration: write files then grep for content."
  (mcp-test-integration-with-dir dir
    ;; Write several files
    (dolist (spec '(("main.js" . "function init() { console.log('start'); }")
                    ("utils.js" . "function helper() { return true; }")
                    ("test.js" . "describe('suite', () => { it('test', () => {}); });")))
      (mcp-server-emacs-tools--write-handler
       `((filePath . ,(expand-file-name (car spec) dir))
         (content . ,(cdr spec)))))
    ;; Grep for function definitions
    (let ((result (mcp-server-emacs-tools--grep-handler
                   `((pattern . "function") (path . ,dir)))))
      (should (string-match-p "Found" result))
      (should (string-match-p "main.js\\|utils.js" result)))))

;;; ============================================================
;;; GLOB → READ MULTIPLE FILES WORKFLOW
;;; ============================================================

(ert-deftest mcp-integration-glob-then-read ()
  "Integration: glob to find files, then read each one."
  (mcp-test-integration-with-dir dir
    ;; Create test files
    (write-region "# File A\ncontent A\n" nil (expand-file-name "doc-a.md" dir) nil 'silent)
    (write-region "# File B\ncontent B\n" nil (expand-file-name "doc-b.md" dir) nil 'silent)
    (write-region "ignored\n" nil (expand-file-name "data.json" dir) nil 'silent)
    ;; Glob for markdown files
    (let ((glob-result (mcp-server-emacs-tools--glob-handler
                        `((pattern . "*.md") (path . ,dir)))))
      (should (string-match-p "\\.md" glob-result))
      ;; Read each found file
      (dolist (line (split-string glob-result "\n" t))
        (when (string-match-p "\\.md$" line)
          (let ((read-result (mcp-server-emacs-tools--read-handler
                              `((filePath . ,line)))))
            (should (string-match-p "File [AB]\\|content" read-result))))))))

;;; ============================================================
;;; MULTIEDIT WORKFLOW
;;; ============================================================

(ert-deftest mcp-integration-multiedit-refactor ()
  "Integration: use multiedit to rename a variable throughout a file."
  (mcp-test-integration-with-dir dir
    (let ((filepath (expand-file-name "code.js" dir)))
      (write-region "const oldName = 1;\nlet x = oldName + 1;\nconsole.log(oldName);\n"
                    nil filepath nil 'silent)
      ;; Multi-edit: rename oldName occurrences sequentially
      (let ((result (mcp-server-emacs-tools--multiedit-handler
                     `((filePath . ,filepath)
                       (edits . [((oldString . "const oldName = 1;")
                                  (newString . "const newName = 1;"))
                                 ((oldString . "let x = oldName + 1;")
                                  (newString . "let x = newName + 1;"))
                                 ((oldString . "console.log(oldName);")
                                  (newString . "console.log(newName);"))])))))
        (should (stringp result))
        (unless (string-match-p "Error" result)
          (let ((content (with-temp-buffer
                           (insert-file-contents filepath)
                           (buffer-string))))
            (should (string-match-p "newName" content))
            (should-not (string-match-p "oldName" content))))))))

;;; ============================================================
;;; APPLY_PATCH MULTI-FILE WORKFLOW
;;; ============================================================

(ert-deftest mcp-integration-apply-patch-multi-file ()
  "Integration: apply patch that adds, modifies, and deletes files."
  (mcp-test-integration-with-dir dir
    (let* ((existing (expand-file-name "existing.txt" dir))
           (to-delete (expand-file-name "delete-me.txt" dir)))
      ;; Set up existing files
      (write-region "original content\n" nil existing nil 'silent)
      (write-region "will be deleted\n" nil to-delete nil 'silent)
      ;; Apply patch
      (let* ((new-file (expand-file-name "brand-new.txt" dir))
             (patch (format "*** Begin Patch\n*** Add File: %s\n+Added by patch\n*** Delete File: %s\n*** End Patch"
                            new-file to-delete))
             (result (mcp-server-emacs-tools--apply-patch-handler
                      `((patchText . ,patch)))))
        (should (string-match-p "Success" result))
        (should (file-exists-p new-file))
        (should-not (file-exists-p to-delete))
        (should (file-exists-p existing))))))

;;; ============================================================
;;; TODO LIFECYCLE WORKFLOW
;;; ============================================================

(ert-deftest mcp-integration-todo-lifecycle ()
  "Integration: full todo lifecycle - create, update, complete."
  ;; Reset
  (setq mcp-server-emacs-tools--todo-list '())
  ;; Create todos
  (mcp-server-emacs-tools--todowrite-handler
   '((todos . [((id . "t1") (content . "Research approach") (status . "pending"))
               ((id . "t2") (content . "Implement feature") (status . "pending"))
               ((id . "t3") (content . "Write tests") (status . "pending"))])))
  (should (= 3 (length mcp-server-emacs-tools--todo-list)))
  ;; Mark first as in_progress
  (mcp-server-emacs-tools--todowrite-handler
   '((todos . [((id . "t1") (content . "Research approach") (status . "in_progress"))
               ((id . "t2") (content . "Implement feature") (status . "pending"))
               ((id . "t3") (content . "Write tests") (status . "pending"))])))
  ;; Read and verify
  (let ((read-result (mcp-server-emacs-tools--todoread-handler '())))
    (should (string-match-p "in_progress" read-result))
    (should (string-match-p "Research" read-result)))
  ;; Complete first, start second
  (mcp-server-emacs-tools--todowrite-handler
   '((todos . [((id . "t1") (content . "Research approach") (status . "completed"))
               ((id . "t2") (content . "Implement feature") (status . "in_progress"))
               ((id . "t3") (content . "Write tests") (status . "pending"))])))
  (let ((final-state mcp-server-emacs-tools--todo-list))
    (let ((t1 (cl-find-if (lambda (t) (string= (alist-get 'id t) "t1")) final-state))
          (t2 (cl-find-if (lambda (t) (string= (alist-get 'id t) "t2")) final-state)))
      (should (string= "completed" (alist-get 'status t1)))
      (should (string= "in_progress" (alist-get 'status t2))))))

;;; ============================================================
;;; BASH → READ WORKFLOW (command output to file)
;;; ============================================================

(ert-deftest mcp-integration-bash-and-read ()
  "Integration: run bash to generate file, then read it."
  (mcp-test-integration-with-dir dir
    (let ((outfile (expand-file-name "output.txt" dir)))
      ;; Write file via bash
      (mcp-server-emacs-tools--bash-handler
       `((command . ,(format "echo 'generated content' > %s" outfile))
         (workdir . ,dir)
         (description . "Generate output file")))
      ;; Read the generated file
      (when (file-exists-p outfile)
        (let ((result (mcp-server-emacs-tools--read-handler
                       `((filePath . ,outfile)))))
          (should (string-match-p "generated content" result)))))))

;;; ============================================================
;;; BATCH TOOL TESTS
;;; ============================================================

(ert-deftest mcp-integration-batch-multiple-reads ()
  "Integration: batch multiple read operations."
  (mcp-test-integration-with-dir dir
    (let ((file-a (expand-file-name "a.txt" dir))
          (file-b (expand-file-name "b.txt" dir)))
      (write-region "content of A\n" nil file-a nil 'silent)
      (write-region "content of B\n" nil file-b nil 'silent)
      ;; Batch read both files
      (let ((result (mcp-server-emacs-tools--batch-handler
                     `((tool_calls . [((tool . "read") (parameters . ((filePath . ,file-a))))
                                       ((tool . "read") (parameters . ((filePath . ,file-b))))])))))
        (should (stringp result))
        (should (string-match-p "Results" result))))))

(ert-deftest mcp-integration-batch-disallows-nested-batch ()
  "Integration: batch cannot call batch."
  (let ((result (mcp-server-emacs-tools--batch-handler
                 '((tool_calls . [((tool . "batch") (parameters . ((tool_calls . []))))])))))
    (should (string-match-p "Error\\|not allowed\\|failed" result))))

;;; ============================================================
;;; LS + GREP COMBINED WORKFLOW
;;; ============================================================

(ert-deftest mcp-integration-ls-and-grep ()
  "Integration: ls a directory and grep within it."
  (mcp-test-integration-with-dir dir
    ;; Create code files
    (write-region "(defun main () (message \"hello\"))\n"
                  nil (expand-file-name "main.el" dir) nil 'silent)
    (write-region "(defun helper () t)\n"
                  nil (expand-file-name "helpers.el" dir) nil 'silent)
    ;; List directory
    (let ((ls-result (mcp-server-emacs-tools--ls-handler `((path . ,dir)))))
      (should (string-match-p "main.el" ls-result))
      (should (string-match-p "helpers.el" ls-result)))
    ;; Grep for defun
    (let ((grep-result (mcp-server-emacs-tools--grep-handler
                        `((pattern . "defun") (path . ,dir)))))
      (should (string-match-p "Found" grep-result))
      (should (string-match-p "main.el\\|helpers.el" grep-result)))))

;;; ============================================================
;;; TASK TOOL TESTS
;;; ============================================================

(ert-deftest mcp-integration-task-creates-record ()
  "Integration: task tool creates a task record."
  (let ((result (mcp-server-emacs-tools--task-handler
                 '((description . "search codebase")
                   (prompt . "Find all uses of the deprecated API")
                   (subagent_type . "researcher")))))
    (should (stringp result))
    (should (string-match-p "task_id:" result))
    (should (string-match-p "task_result" result))))

(ert-deftest mcp-integration-task-resume-with-id ()
  "Integration: task tool can resume with an existing task_id."
  (let* ((first-result (mcp-server-emacs-tools--task-handler
                        '((description . "initial task")
                          (prompt . "Do something")
                          (subagent_type . "coder"))))
         ;; Extract task_id
         (task-id (when (string-match "task_id: \\([^\n]+\\)" first-result)
                    (match-string 1 first-result))))
    (when task-id
      (let ((resume-result (mcp-server-emacs-tools--task-handler
                            `((description . "continued task")
                              (prompt . "Continue from before")
                              (subagent_type . "coder")
                              (task_id . ,task-id)))))
        (should (stringp resume-result))
        (should (string-match-p "task_result" resume-result))))))

;;; ============================================================
;;; SKILL TOOL TESTS
;;; ============================================================

(ert-deftest mcp-integration-skill-not-found ()
  "Integration: skill tool returns error for unknown skill."
  (let ((result (mcp-server-emacs-tools--skill-handler
                 '((name . "nonexistent-skill-xyz")))))
    (should (string-match-p "Error\\|not found" result))))

(ert-deftest mcp-integration-skill-tool-registered ()
  "Test that skill tool is registered."
  (should (mcp-server-tools-exists-p "skill")))

;;; ============================================================
;;; MCP FRAMEWORK INTEGRATION TESTS
;;; ============================================================

(ert-deftest mcp-integration-tool-call-via-framework ()
  "Integration: call tools via the MCP framework's mcp-server-tools-call."
  ;; The framework should properly wrap tool results
  (let ((result (mcp-server-tools-call "todoread" '())))
    ;; Result should be a vector of content items
    (should (vectorp result))
    (when (> (length result) 0)
      (let ((item (aref result 0)))
        (should (alist-get 'type item))
        (should (alist-get 'text item))))))

(ert-deftest mcp-integration-all-tools-callable ()
  "Integration: verify all registered tools can be introspected."
  (let ((tools (mcp-server-tools-list)))
    (should (listp tools))
    ;; Verify each registered tool has required fields
    (dolist (tool tools)
      (should (alist-get 'name tool))
      (should (alist-get 'description tool))
      (should (alist-get 'inputSchema tool)))))

(provide 'test-mcp-opencode-tools-integration)

;;; test-mcp-opencode-tools-integration.el ends here
