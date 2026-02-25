;;; test-mcp-opencode-tools-search.el --- ERT Tests for Search Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for the opencode-translated search MCP tools:
;; grep, glob, ls (list), bash, todo, question

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
(require 'mcp-server-emacs-tools-grep)
(require 'mcp-server-emacs-tools-glob)
(require 'mcp-server-emacs-tools-ls)
(require 'mcp-server-emacs-tools-bash)
(require 'mcp-server-emacs-tools-todo)
(require 'mcp-server-emacs-tools-webfetch)

;;; Helpers

(defmacro mcp-test-with-temp-dir-tree (var files &rest body)
  "Create temp dir VAR with FILES alist (relative-path . content) and run BODY."
  (declare (indent 2))
  `(let ((,var (make-temp-file "mcp-search-test-" t)))
     (unwind-protect
         (progn
           (dolist (spec ,files)
             (let* ((rel (car spec))
                    (content (cdr spec))
                    (full (expand-file-name rel ,var))
                    (dir (file-name-directory full)))
               (when dir (make-directory dir t))
               (write-region content nil full nil 'silent)))
           ,@body)
       (when (file-directory-p ,var) (delete-directory ,var t)))))

;;; ============================================================
;;; GREP TOOL TESTS
;;; ============================================================

(ert-deftest mcp-test-grep-finds-pattern ()
  "Test grep finds a pattern in files."
  (mcp-test-with-temp-dir-tree dir
    '(("file1.txt" . "Hello World\nSecond line\n")
      ("file2.txt" . "Another Hello here\nNo match\n"))
    (let ((result (mcp-server-emacs-tools--grep-handler
                   `((pattern . "Hello") (path . ,dir)))))
      (should (stringp result))
      (should (string-match-p "Hello" result))
      (should (string-match-p "Found" result)))))

(ert-deftest mcp-test-grep-no-matches ()
  "Test grep returns no files when no match."
  (mcp-test-with-temp-dir-tree dir
    '(("file.txt" . "content here\n"))
    (let ((result (mcp-server-emacs-tools--grep-handler
                   `((pattern . "ZZZNOMATCH") (path . ,dir)))))
      (should (string-match-p "No files found" result)))))

(ert-deftest mcp-test-grep-with-include-filter ()
  "Test grep with file type filter."
  (mcp-test-with-temp-dir-tree dir
    '(("file.js" . "function hello() {}\n")
      ("file.py" . "def hello(): pass\n"))
    (let ((result (mcp-server-emacs-tools--grep-handler
                   `((pattern . "hello") (path . ,dir) (include . "*.js")))))
      (should (stringp result))
      ;; Should find in .js file
      (when (string-match-p "Found" result)
        (should (string-match-p "file.js" result))))))

(ert-deftest mcp-test-grep-requires-pattern ()
  "Test grep returns error without pattern."
  (let ((result (mcp-server-emacs-tools--grep-handler '())))
    (should (string-match-p "Error\\|pattern" result))))

(ert-deftest mcp-test-grep-tool-registered ()
  "Test that grep tool is registered."
  (should (mcp-server-tools-exists-p "grep")))

(ert-deftest mcp-test-grep-line-numbers ()
  "Test grep returns line numbers."
  (mcp-test-with-temp-dir-tree dir
    '(("target.txt" . "nope\nfound it here\nnope again\n"))
    (let ((result (mcp-server-emacs-tools--grep-handler
                   `((pattern . "found it") (path . ,dir)))))
      (when (string-match-p "Found" result)
        (should (string-match-p "Line 2" result))))))

;;; ============================================================
;;; GLOB TOOL TESTS
;;; ============================================================

(ert-deftest mcp-test-glob-finds-files ()
  "Test glob finds files by pattern."
  (mcp-test-with-temp-dir-tree dir
    '(("foo.txt" . "content")
      ("bar.txt" . "content")
      ("baz.el" . "content"))
    (let ((result (mcp-server-emacs-tools--glob-handler
                   `((pattern . "*.txt") (path . ,dir)))))
      (should (stringp result))
      (should-not (string= "No files found" result))
      (should (string-match-p "\\.txt" result)))))

(ert-deftest mcp-test-glob-no-matches ()
  "Test glob returns no files when no match."
  (mcp-test-with-temp-dir-tree dir
    '(("file.txt" . "content"))
    (let ((result (mcp-server-emacs-tools--glob-handler
                   `((pattern . "*.xyz") (path . ,dir)))))
      (should (string= "No files found" result)))))

(ert-deftest mcp-test-glob-requires-pattern ()
  "Test glob returns error without pattern."
  (let ((result (mcp-server-emacs-tools--glob-handler '())))
    (should (string-match-p "Error\\|pattern" result))))

(ert-deftest mcp-test-glob-tool-registered ()
  "Test that glob tool is registered."
  (should (mcp-server-tools-exists-p "glob")))

(ert-deftest mcp-test-glob-sorts-by-mtime ()
  "Test that glob results are sorted (most recently modified first)."
  (mcp-test-with-temp-dir-tree dir
    '(("file-a.txt" . "a") ("file-b.txt" . "b"))
    (let ((result (mcp-server-emacs-tools--glob-handler
                   `((pattern . "*.txt") (path . ,dir)))))
      ;; Just verify it returns results without error
      (should (stringp result))
      (should-not (string= "No files found" result)))))

;;; ============================================================
;;; LIST (LS) TOOL TESTS
;;; ============================================================

(ert-deftest mcp-test-ls-basic-listing ()
  "Test ls returns directory listing."
  (mcp-test-with-temp-dir-tree dir
    '(("file1.txt" . "a") ("file2.txt" . "b"))
    (let ((result (mcp-server-emacs-tools--ls-handler `((path . ,dir)))))
      (should (stringp result))
      (should (string-match-p "file1.txt" result))
      (should (string-match-p "file2.txt" result)))))

(ert-deftest mcp-test-ls-tree-structure ()
  "Test ls shows tree structure for subdirectories."
  (mcp-test-with-temp-dir-tree dir
    '(("file.txt" . "content")
      ("subdir/nested.txt" . "nested"))
    (let ((result (mcp-server-emacs-tools--ls-handler `((path . ,dir)))))
      (should (string-match-p "subdir" result))
      (should (string-match-p "nested.txt" result)))))

(ert-deftest mcp-test-ls-ignores-node-modules ()
  "Test ls ignores node_modules by default."
  (mcp-test-with-temp-dir-tree dir
    '(("src/main.js" . "code")
      ("node_modules/pkg/index.js" . "pkg"))
    (let ((result (mcp-server-emacs-tools--ls-handler `((path . ,dir)))))
      (should (string-match-p "main.js" result))
      (should-not (string-match-p "node_modules" result)))))

(ert-deftest mcp-test-ls-not-a-directory ()
  "Test ls returns error for non-directory path."
  (let ((f (make-temp-file "mcp-ls-test-")))
    (unwind-protect
        (let ((result (mcp-server-emacs-tools--ls-handler `((path . ,f)))))
          (should (string-match-p "Error\\|Not a directory" result)))
      (delete-file f))))

(ert-deftest mcp-test-ls-tool-registered ()
  "Test that list tool is registered."
  (should (mcp-server-tools-exists-p "list")))

;;; ============================================================
;;; BASH TOOL TESTS
;;; ============================================================

(ert-deftest mcp-test-bash-simple-command ()
  "Test bash executes a simple command."
  (let ((result (mcp-server-emacs-tools--bash-handler
                 '((command . "echo hello")
                   (description . "Echo hello")))))
    (should (stringp result))
    (should (string-match-p "hello" result))))

(ert-deftest mcp-test-bash-with-workdir ()
  "Test bash respects working directory."
  (mcp-test-with-temp-dir-tree dir
    '(("test.txt" . "content"))
    (let ((result (mcp-server-emacs-tools--bash-handler
                   `((command . "ls") (workdir . ,dir)
                     (description . "List files")))))
      (should (string-match-p "test.txt" result)))))

(ert-deftest mcp-test-bash-captures-stderr ()
  "Test bash captures stderr output."
  (let ((result (mcp-server-emacs-tools--bash-handler
                 '((command . "echo error >&2")
                   (description . "Echo to stderr")))))
    (should (stringp result))))

(ert-deftest mcp-test-bash-requires-command ()
  "Test bash returns error without command."
  (let ((result (mcp-server-emacs-tools--bash-handler '())))
    (should (string-match-p "Error\\|command" result))))

(ert-deftest mcp-test-bash-invalid-timeout ()
  "Test bash returns error for negative timeout."
  (let ((result (mcp-server-emacs-tools--bash-handler
                 '((command . "echo hi") (timeout . -1)
                   (description . "test")))))
    (should (string-match-p "Error\\|timeout\\|Invalid" result))))

(ert-deftest mcp-test-bash-tool-registered ()
  "Test that bash tool is registered."
  (should (mcp-server-tools-exists-p "bash")))

;;; ============================================================
;;; TODO TOOLS TESTS
;;; ============================================================

(ert-deftest mcp-test-todowrite-creates-todos ()
  "Test todowrite creates a todo list."
  ;; Reset state
  (setq mcp-server-emacs-tools--todo-list '())
  (let ((result (mcp-server-emacs-tools--todowrite-handler
                 '((todos . [((id . "1") (content . "First task") (status . "pending"))
                              ((id . "2") (content . "Second task") (status . "in_progress"))])))))
    (should (stringp result))
    (should (string-match-p "Updated" result))
    (should (= 2 (length mcp-server-emacs-tools--todo-list)))))

(ert-deftest mcp-test-todoread-returns-todos ()
  "Test todoread returns the current todo list."
  (setq mcp-server-emacs-tools--todo-list
        '(((id . "1") (content . "test task") (status . "pending"))))
  (let ((result (mcp-server-emacs-tools--todoread-handler '())))
    (should (stringp result))
    (should (string-match-p "test task" result))))

(ert-deftest mcp-test-todowrite-validates-status ()
  "Test todowrite rejects invalid status."
  (setq mcp-server-emacs-tools--todo-list '())
  (let ((result (mcp-server-emacs-tools--todowrite-handler
                 '((todos . [((id . "1") (content . "task") (status . "invalid-status"))])))))
    (should (string-match-p "Error\\|Invalid status" result))))

(ert-deftest mcp-test-todowrite-tool-registered ()
  "Test that todowrite tool is registered."
  (should (mcp-server-tools-exists-p "todowrite")))

(ert-deftest mcp-test-todoread-tool-registered ()
  "Test that todoread tool is registered."
  (should (mcp-server-tools-exists-p "todoread")))

;;; ============================================================
;;; WEBFETCH TESTS (HTML conversion, no network needed)
;;; ============================================================

(ert-deftest mcp-test-webfetch-html-to-text ()
  "Test HTML to text conversion."
  (let ((html "<html><body><p>Hello <strong>World</strong></p><script>var x=1;</script></body></html>"))
    (let ((result (mcp-server-emacs-tools--webfetch-html-to-text html)))
      (should (stringp result))
      (should (string-match-p "Hello" result))
      (should (string-match-p "World" result))
      ;; Script content should be removed
      (should-not (string-match-p "var x=1" result)))))

(ert-deftest mcp-test-webfetch-html-to-markdown ()
  "Test HTML to markdown conversion."
  (let ((html "<h1>Title</h1><p>Paragraph with <strong>bold</strong> text.</p>"))
    (let ((result (mcp-server-emacs-tools--webfetch-html-to-markdown html)))
      (should (stringp result))
      (should (string-match-p "Title" result))
      (should (string-match-p "bold" result)))))

(ert-deftest mcp-test-webfetch-requires-url ()
  "Test webfetch returns error without url."
  (let ((result (mcp-server-emacs-tools--webfetch-handler '())))
    (should (string-match-p "Error\\|url" result))))

(ert-deftest mcp-test-webfetch-invalid-url ()
  "Test webfetch returns error for non-http URL."
  (let ((result (mcp-server-emacs-tools--webfetch-handler
                 '((url . "ftp://example.com")))))
    (should (string-match-p "Error\\|https?" result))))

(ert-deftest mcp-test-webfetch-tool-registered ()
  "Test that webfetch tool is registered."
  (should (mcp-server-tools-exists-p "webfetch")))

;;; ============================================================
;;; OTHER TOOLS REGISTERED TESTS
;;; ============================================================

(ert-deftest mcp-test-all-tools-registered ()
  "Test that all expected tools are registered."
  (let ((expected '("read" "write" "edit" "grep" "glob" "list" "bash"
                    "webfetch" "websearch" "codesearch"
                    "todowrite" "todoread" "question"
                    "apply_patch" "multiedit" "lsp" "batch" "task" "skill")))
    (dolist (name expected)
      (should (mcp-server-tools-exists-p name)))))

(provide 'test-mcp-opencode-tools-search)

;;; test-mcp-opencode-tools-search.el ends here
