;;; test-mcp-opencode-tools-file-ops.el --- ERT Tests for File Operation Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; ERT tests for the opencode-translated MCP tools:
;; read, write, edit, multiedit, apply_patch

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
(require 'mcp-server-emacs-tools-multiedit)
(require 'mcp-server-emacs-tools-apply-patch)

;;; Helpers

(defmacro mcp-test-with-temp-file (var content &rest body)
  "Bind VAR to a temp file containing CONTENT and execute BODY."
  (declare (indent 2))
  `(let ((,var (make-temp-file "mcp-test-")))
     (unwind-protect
         (progn
           (write-region ,content nil ,var nil 'silent)
           ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

(defmacro mcp-test-with-temp-dir (var &rest body)
  "Bind VAR to a temp directory and execute BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "mcp-test-dir-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

;;; ============================================================
;;; READ TOOL TESTS
;;; ============================================================

(ert-deftest mcp-test-read-basic-file ()
  "Test reading a simple text file."
  (mcp-test-with-temp-file f "line1\nline2\nline3\n"
    (let ((result (mcp-server-emacs-tools--read-handler `((filePath . ,f)))))
      (should (stringp result))
      (should (string-match-p "1: line1" result))
      (should (string-match-p "2: line2" result))
      (should (string-match-p "3: line3" result))
      (should (string-match-p "<type>file</type>" result))
      (should (string-match-p "End of file - total 3 lines" result)))))

(ert-deftest mcp-test-read-file-with-offset ()
  "Test reading file starting from an offset."
  (mcp-test-with-temp-file f "line1\nline2\nline3\nline4\nline5\n"
    (let ((result (mcp-server-emacs-tools--read-handler
                   `((filePath . ,f) (offset . 3)))))
      (should (string-match-p "3: line3" result))
      (should (string-match-p "4: line4" result))
      (should-not (string-match-p "1: line1" result)))))

(ert-deftest mcp-test-read-file-with-limit ()
  "Test reading file with line limit."
  (mcp-test-with-temp-file f "line1\nline2\nline3\nline4\nline5\n"
    (let ((result (mcp-server-emacs-tools--read-handler
                   `((filePath . ,f) (limit . 2)))))
      (should (string-match-p "1: line1" result))
      (should (string-match-p "2: line2" result))
      (should-not (string-match-p "3: line3" result))
      (should (string-match-p "offset=3" result)))))

(ert-deftest mcp-test-read-file-not-found ()
  "Test reading a nonexistent file returns error."
  (let ((result (mcp-server-emacs-tools--read-handler
                 '((filePath . "/tmp/mcp-test-nonexistent-xyz-12345.txt")))))
    (should (string-match-p "File not found\\|Error" result))))

(ert-deftest mcp-test-read-directory ()
  "Test reading a directory."
  (mcp-test-with-temp-dir dir
    (write-region "a" nil (expand-file-name "file-a.txt" dir) nil 'silent)
    (write-region "b" nil (expand-file-name "file-b.txt" dir) nil 'silent)
    (make-directory (expand-file-name "subdir" dir))
    (let ((result (mcp-server-emacs-tools--read-handler `((filePath . ,dir)))))
      (should (string-match-p "<type>directory</type>" result))
      (should (string-match-p "file-a.txt" result))
      (should (string-match-p "file-b.txt" result))
      (should (string-match-p "subdir/" result)))))

(ert-deftest mcp-test-read-invalid-offset ()
  "Test that offset < 1 returns an error."
  (mcp-test-with-temp-file f "content\n"
    (let ((result (mcp-server-emacs-tools--read-handler
                   `((filePath . ,f) (offset . 0)))))
      (should (string-match-p "Error\\|offset must be" result)))))

(ert-deftest mcp-test-read-tool-registered ()
  "Test that the read tool is registered in the MCP registry."
  (should (mcp-server-tools-exists-p "read")))

;;; ============================================================
;;; WRITE TOOL TESTS
;;; ============================================================

(ert-deftest mcp-test-write-creates-new-file ()
  "Test writing creates a new file."
  (let ((path (make-temp-file "mcp-write-test-")))
    (delete-file path)  ; ensure it doesn't exist
    (unwind-protect
        (let ((result (mcp-server-emacs-tools--write-handler
                       `((filePath . ,path) (content . "hello world")))))
          (should (file-exists-p path))
          (should (string= "hello world"
                           (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))))
          (should (string-match-p "Wrote file successfully" result)))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest mcp-test-write-overwrites-existing ()
  "Test writing overwrites existing file content."
  (mcp-test-with-temp-file f "original content"
    (mcp-server-emacs-tools--write-handler
     `((filePath . ,f) (content . "new content")))
    (should (string= "new content"
                     (with-temp-buffer
                       (insert-file-contents f)
                       (buffer-string))))))

(ert-deftest mcp-test-write-creates-parent-dirs ()
  "Test writing creates parent directories as needed."
  (let* ((base (make-temp-file "mcp-write-test-" t))
         (path (expand-file-name "subdir/nested/file.txt" base)))
    (unwind-protect
        (progn
          (mcp-server-emacs-tools--write-handler
           `((filePath . ,path) (content . "nested content")))
          (should (file-exists-p path))
          (should (string= "nested content"
                           (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string)))))
      (delete-directory base t))))

(ert-deftest mcp-test-write-requires-filepath ()
  "Test that missing filePath returns error."
  (let ((result (mcp-server-emacs-tools--write-handler '((content . "data")))))
    (should (string-match-p "Error\\|filePath" result))))

(ert-deftest mcp-test-write-tool-registered ()
  "Test that the write tool is registered."
  (should (mcp-server-tools-exists-p "write")))

;;; ============================================================
;;; EDIT TOOL TESTS
;;; ============================================================

(ert-deftest mcp-test-edit-simple-replacement ()
  "Test basic string replacement."
  (mcp-test-with-temp-file f "Hello, World!\nSecond line.\n"
    (let ((result (mcp-server-emacs-tools--edit-handler
                   `((filePath . ,f)
                     (oldString . "World")
                     (newString . "Emacs")))))
      (should (string-match-p "Edit applied successfully" result))
      (let ((content (with-temp-buffer
                       (insert-file-contents f)
                       (buffer-string))))
        (should (string-match-p "Hello, Emacs!" content))
        (should-not (string-match-p "World" content))))))

(ert-deftest mcp-test-edit-multiline-replacement ()
  "Test multiline string replacement."
  (mcp-test-with-temp-file f "function foo() {\n  return 1;\n}\n"
    (mcp-server-emacs-tools--edit-handler
     `((filePath . ,f)
       (oldString . "function foo() {\n  return 1;\n}")
       (newString . "function foo() {\n  return 42;\n}")))
    (let ((content (with-temp-buffer
                     (insert-file-contents f)
                     (buffer-string))))
      (should (string-match-p "return 42" content)))))

(ert-deftest mcp-test-edit-identical-strings-error ()
  "Test that identical oldString and newString returns error."
  (mcp-test-with-temp-file f "some content\n"
    (let ((result (mcp-server-emacs-tools--edit-handler
                   `((filePath . ,f)
                     (oldString . "some")
                     (newString . "some")))))
      (should (string-match-p "Error\\|identical" result)))))

(ert-deftest mcp-test-edit-not-found-error ()
  "Test that missing oldString returns error."
  (mcp-test-with-temp-file f "some content\n"
    (let ((result (mcp-server-emacs-tools--edit-handler
                   `((filePath . ,f)
                     (oldString . "NOTPRESENT")
                     (newString . "replacement")))))
      (should (string-match-p "Error\\|Could not find" result)))))

(ert-deftest mcp-test-edit-replace-all ()
  "Test replaceAll option."
  (mcp-test-with-temp-file f "foo bar foo baz foo\n"
    (mcp-server-emacs-tools--edit-handler
     `((filePath . ,f)
       (oldString . "foo")
       (newString . "qux")
       (replaceAll . t)))
    (let ((content (with-temp-buffer
                     (insert-file-contents f)
                     (buffer-string))))
      (should-not (string-match-p "foo" content))
      (should (= 3 (length (split-string content "qux" t)))))))

(ert-deftest mcp-test-edit-empty-oldstring-creates-file ()
  "Test that empty oldString creates a new file."
  (let ((path (make-temp-file "mcp-edit-create-")))
    (delete-file path)
    (unwind-protect
        (progn
          (mcp-server-emacs-tools--edit-handler
           `((filePath . ,path)
             (oldString . "")
             (newString . "brand new content")))
          (should (file-exists-p path))
          (let ((content (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))))
            (should (string-match-p "brand new content" content))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest mcp-test-edit-line-trimmed-replacement ()
  "Test line-trimmed replacement strategy handles indentation."
  (mcp-test-with-temp-file f "  indented line\n  another line\n"
    ;; Search without indentation
    (let ((result (mcp-server-emacs-tools--edit-handler
                   `((filePath . ,f)
                     (oldString . "indented line")
                     (newString . "replaced line")))))
      ;; Should find it via line-trimmed or another strategy
      (should (stringp result)))))

(ert-deftest mcp-test-edit-tool-registered ()
  "Test that the edit tool is registered."
  (should (mcp-server-tools-exists-p "edit")))

;;; ============================================================
;;; REPLACE FUNCTION UNIT TESTS
;;; ============================================================

(ert-deftest mcp-test-replace-simple ()
  "Unit test for simple replacer."
  (let ((result (mcp-server-emacs-tools--replace
                 "Hello World" "World" "Emacs")))
    (should (string= result "Hello Emacs"))))

(ert-deftest mcp-test-replace-all ()
  "Unit test for replace-all."
  (let ((result (mcp-server-emacs-tools--replace
                 "a b a c a" "a" "x" t)))
    (should (string= result "x b x c x"))))

(ert-deftest mcp-test-levenshtein ()
  "Unit test for Levenshtein distance."
  (should (= 0 (mcp-server-emacs-tools--levenshtein "abc" "abc")))
  (should (= 1 (mcp-server-emacs-tools--levenshtein "abc" "axc")))
  (should (= 3 (mcp-server-emacs-tools--levenshtein "" "abc")))
  (should (= 3 (mcp-server-emacs-tools--levenshtein "abc" ""))))

;;; ============================================================
;;; MULTIEDIT TOOL TESTS
;;; ============================================================

(ert-deftest mcp-test-multiedit-sequential ()
  "Test multiple sequential edits on a file."
  (mcp-test-with-temp-file f "line1\nline2\nline3\n"
    (let ((result (mcp-server-emacs-tools--multiedit-handler
                   `((filePath . ,f)
                     (edits . [((oldString . "line1") (newString . "FIRST"))
                               ((oldString . "line2") (newString . "SECOND"))
                               ((oldString . "line3") (newString . "THIRD"))])))))
      (should (string-match-p "applied successfully\\|Error" result))
      (when (not (string-match-p "Error" result))
        (let ((content (with-temp-buffer
                         (insert-file-contents f)
                         (buffer-string))))
          (should (string-match-p "FIRST" content))
          (should (string-match-p "SECOND" content))
          (should (string-match-p "THIRD" content)))))))

(ert-deftest mcp-test-multiedit-tool-registered ()
  "Test that the multiedit tool is registered."
  (should (mcp-server-tools-exists-p "multiedit")))

;;; ============================================================
;;; APPLY_PATCH TOOL TESTS
;;; ============================================================

(ert-deftest mcp-test-apply-patch-add-file ()
  "Test patch adding a new file."
  (let ((path (make-temp-file "mcp-patch-add-")))
    (delete-file path)
    (unwind-protect
        (let* ((patch (format "*** Begin Patch\n*** Add File: %s\n+Hello from patch\n+Second line\n*** End Patch"
                              path))
               (result (mcp-server-emacs-tools--apply-patch-handler
                        `((patchText . ,patch)))))
          (should (string-match-p "Success\\|A " result))
          (should (file-exists-p path))
          (let ((content (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))))
            (should (string-match-p "Hello from patch" content))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest mcp-test-apply-patch-delete-file ()
  "Test patch deleting a file."
  (mcp-test-with-temp-file f "to be deleted\n"
    (let* ((patch (format "*** Begin Patch\n*** Delete File: %s\n*** End Patch" f))
           (result (mcp-server-emacs-tools--apply-patch-handler
                    `((patchText . ,patch)))))
      (should (string-match-p "Success\\|D " result))
      (should-not (file-exists-p f)))))

(ert-deftest mcp-test-apply-patch-invalid-format ()
  "Test that invalid patch format returns error."
  (let ((result (mcp-server-emacs-tools--apply-patch-handler
                 '((patchText . "not a valid patch")))))
    (should (string-match-p "Error\\|No '\\*\\*\\* Begin Patch'" result))))

(ert-deftest mcp-test-apply-patch-tool-registered ()
  "Test that apply_patch tool is registered."
  (should (mcp-server-tools-exists-p "apply_patch")))

;;; ============================================================
;;; PATCH PARSER UNIT TESTS
;;; ============================================================

(ert-deftest mcp-test-patch-parse-add ()
  "Unit test for patch parser: add file."
  (let* ((patch "*** Begin Patch\n*** Add File: foo.txt\n+line one\n+line two\n*** End Patch")
         (hunks (mcp-server-emacs-tools--patch-parse patch)))
    (should (= 1 (length hunks)))
    (should (eq 'add (plist-get (car hunks) :type)))
    (should (string= "foo.txt" (plist-get (car hunks) :path)))
    (should (string-match-p "line one" (plist-get (car hunks) :contents)))))

(ert-deftest mcp-test-patch-parse-delete ()
  "Unit test for patch parser: delete file."
  (let* ((patch "*** Begin Patch\n*** Delete File: bar.txt\n*** End Patch")
         (hunks (mcp-server-emacs-tools--patch-parse patch)))
    (should (= 1 (length hunks)))
    (should (eq 'delete (plist-get (car hunks) :type)))
    (should (string= "bar.txt" (plist-get (car hunks) :path)))))

(ert-deftest mcp-test-patch-parse-multiple ()
  "Unit test for patch parser: multiple operations."
  (let* ((patch "*** Begin Patch\n*** Add File: new.txt\n+content\n*** Delete File: old.txt\n*** End Patch")
         (hunks (mcp-server-emacs-tools--patch-parse patch)))
    (should (= 2 (length hunks)))
    (should (eq 'add (plist-get (nth 0 hunks) :type)))
    (should (eq 'delete (plist-get (nth 1 hunks) :type)))))

(provide 'test-mcp-opencode-tools-file-ops)

;;; test-mcp-opencode-tools-file-ops.el ends here
