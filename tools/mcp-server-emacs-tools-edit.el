;;; mcp-server-emacs-tools-edit.el --- Edit File MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for performing exact string replacements in files.
;; Mirrors the opencode edit.ts tool with multiple fuzzy matching strategies:
;; SimpleReplacer, LineTrimmedReplacer, BlockAnchorReplacer,
;; WhitespaceNormalizedReplacer, IndentationFlexibleReplacer,
;; EscapeNormalizedReplacer, TrimmedBoundaryReplacer, ContextAwareReplacer,
;; MultiOccurrenceReplacer.

;;; Code:

(require 'mcp-server-tools)
(require 'cl-lib)

;;; Levenshtein distance

(defun mcp-server-emacs-tools--levenshtein (a b)
  "Compute Levenshtein distance between strings A and B."
  (let* ((la (length a))
         (lb (length b))
         (matrix (make-vector (1+ la) nil)))
    (dotimes (i (1+ la))
      (aset matrix i (make-vector (1+ lb) 0))
      (aset (aref matrix i) 0 i))
    (dotimes (j (1+ lb))
      (aset (aref matrix 0) j j))
    (dotimes (i la)
      (dotimes (j lb)
        (let ((cost (if (equal (aref a i) (aref b j)) 0 1)))
          (aset (aref matrix (1+ i)) (1+ j)
                (min (1+ (aref (aref matrix i) (1+ j)))
                     (1+ (aref (aref matrix (1+ i)) j))
                     (+ (aref (aref matrix i) j) cost))))))
    (aref (aref matrix la) lb)))

;;; Replacers - each returns a list of candidate match strings

(defun mcp-server-emacs-tools--simple-replacer (content find)
  "Return FIND if present in CONTENT (exact match)."
  (when (string-match-p (regexp-quote find) content)
    (list find)))

(defun mcp-server-emacs-tools--line-trimmed-replacer (content find)
  "Find FIND in CONTENT by matching trimmed lines."
  (let* ((orig-lines (split-string content "\n" nil))
         (search-lines (split-string find "\n" nil))
         (search-lines (if (and search-lines (string= (car (last search-lines)) ""))
                           (butlast search-lines)
                         search-lines))
         (slen (length search-lines))
         (olen (length orig-lines))
         (results '()))
    (cl-loop for i from 0 to (- olen slen) do
      (let ((matches t))
        (cl-loop for j from 0 to (1- slen) do
          (unless (string= (string-trim (nth (+ i j) orig-lines))
                           (string-trim (nth j search-lines)))
            (setq matches nil)))
        (when matches
          ;; Reconstruct the actual match from original lines
          (let* ((match-lines (cl-subseq orig-lines i (+ i slen)))
                 (match (mapconcat #'identity match-lines "\n")))
            (push match results)))))
    (nreverse results)))

(defun mcp-server-emacs-tools--block-anchor-replacer (content find)
  "Find FIND in CONTENT using first/last line anchors with similarity scoring."
  (let* ((search-lines (split-string find "\n" nil))
         (search-lines (if (and search-lines (string= (car (last search-lines)) ""))
                           (butlast search-lines)
                         search-lines))
         (slen (length search-lines)))
    (when (< slen 3) (cl-return-from mcp-server-emacs-tools--block-anchor-replacer nil))
    (let* ((orig-lines (split-string content "\n" nil))
           (olen (length orig-lines))
           (first-search (string-trim (car search-lines)))
           (last-search (string-trim (car (last search-lines))))
           (candidates '()))
      ;; Collect candidate positions
      (cl-loop for i from 0 to (1- olen) do
        (when (string= (string-trim (nth i orig-lines)) first-search)
          (cl-loop for j from (+ i 2) to (1- olen) do
            (when (string= (string-trim (nth j orig-lines)) last-search)
              (push (list i j) candidates)
              (cl-return)))))
      (when (null candidates) (cl-return-from mcp-server-emacs-tools--block-anchor-replacer nil))
      (if (= (length candidates) 1)
          ;; Single candidate - relaxed threshold
          (let* ((cand (car candidates))
                 (start (car cand))
                 (end (cadr cand))
                 (match-lines (cl-subseq orig-lines start (1+ end))))
            (list (mapconcat #'identity match-lines "\n")))
        ;; Multiple candidates - pick best by similarity
        (let ((best nil) (max-sim -1))
          (dolist (cand candidates)
            (let* ((start (car cand))
                   (end (cadr cand))
                   (actual-lines (cl-subseq orig-lines start (1+ end)))
                   (lines-to-check (min (- slen 2) (- (length actual-lines) 2)))
                   (sim 0))
              (when (> lines-to-check 0)
                (cl-loop for j from 1 to (min (1- slen) (1- (length actual-lines)))
                          when (< j (1- (length actual-lines)))
                          do
                  (let* ((ol (string-trim (nth (+ start j) orig-lines)))
                         (sl (string-trim (nth j search-lines)))
                         (maxlen (max (length ol) (length sl))))
                    (when (> maxlen 0)
                      (setq sim (+ sim (- 1 (/ (float (mcp-server-emacs-tools--levenshtein ol sl))
                                                maxlen))))))))
              (when (> lines-to-check 0) (setq sim (/ sim lines-to-check)))
              (when (> sim max-sim)
                (setq max-sim sim)
                (setq best cand))))
          (when (and best (>= max-sim 0.3))
            (let* ((start (car best))
                   (end (cadr best))
                   (match-lines (cl-subseq orig-lines start (1+ end))))
              (list (mapconcat #'identity match-lines "\n")))))))))

(defun mcp-server-emacs-tools--normalize-whitespace (s)
  "Normalize whitespace in S: collapse runs to single space, trim."
  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " s)))

(defun mcp-server-emacs-tools--whitespace-normalized-replacer (content find)
  "Find FIND in CONTENT by normalizing whitespace."
  (let* ((norm-find (mcp-server-emacs-tools--normalize-whitespace find))
         (lines (split-string content "\n" nil))
         (results '()))
    ;; Single-line matches
    (dolist (line lines)
      (when (string= (mcp-server-emacs-tools--normalize-whitespace line) norm-find)
        (push line results)))
    ;; Multi-line matches
    (let* ((find-lines (split-string find "\n" nil))
           (flen (length find-lines))
           (olen (length lines)))
      (when (> flen 1)
        (cl-loop for i from 0 to (- olen flen) do
          (let ((block (mapconcat #'identity (cl-subseq lines i (+ i flen)) "\n")))
            (when (string= (mcp-server-emacs-tools--normalize-whitespace block) norm-find)
              (push block results))))))
    (nreverse results)))

(defun mcp-server-emacs-tools--remove-indentation (text)
  "Remove common leading indentation from TEXT."
  (let* ((lines (split-string text "\n" nil))
         (non-empty (seq-filter (lambda (l) (> (length (string-trim l)) 0)) lines))
         (min-indent (if non-empty
                         (apply #'min (mapcar (lambda (l)
                                                (let ((m (string-match "^\\([[:space:]]*\\)" l)))
                                                  (if m (length (match-string 1 l)) 0)))
                                              non-empty))
                       0)))
    (mapconcat (lambda (l) (if (> (length (string-trim l)) 0) (substring l (min min-indent (length l))) l))
               lines "\n")))

(defun mcp-server-emacs-tools--min-indent (text)
  "Return the minimum indentation level in TEXT (non-empty lines only)."
  (let* ((lines (split-string text "\n" nil))
         (non-empty (seq-filter (lambda (l) (> (length (string-trim l)) 0)) lines)))
    (if non-empty
        (apply #'min (mapcar (lambda (l)
                               (let ((m (string-match "^\\([[:space:]]*\\)" l)))
                                 (if m (length (match-string 1 l)) 0)))
                             non-empty))
      0)))

(defun mcp-server-emacs-tools--strip-indent (text n)
  "Remove N leading characters from each non-empty line in TEXT."
  (mapconcat (lambda (l) (if (>= (length l) n) (substring l n) l))
             (split-string text "\n" nil) "\n"))

(defun mcp-server-emacs-tools--indentation-flexible-replacer (content find)
  "Find FIND in CONTENT ignoring indentation differences."
  (let* ((find-lines (split-string find "\n" nil))
         (flen (length find-lines))
         (content-lines (split-string content "\n" nil))
         (clen (length content-lines))
         (norm-find (mcp-server-emacs-tools--strip-indent find (mcp-server-emacs-tools--min-indent find)))
         (results '()))
    (cl-loop for i from 0 to (- clen flen) do
      (let* ((block (mapconcat #'identity (cl-subseq content-lines i (+ i flen)) "\n"))
             (norm-block (mcp-server-emacs-tools--strip-indent block (mcp-server-emacs-tools--min-indent block))))
        (when (string= norm-block norm-find)
          (push block results))))
    (nreverse results)))

(defun mcp-server-emacs-tools--unescape-string (s)
  "Unescape common escape sequences in S."
  (let ((result s))
    (setq result (replace-regexp-in-string "\\\\n" "\n" result))
    (setq result (replace-regexp-in-string "\\\\t" "\t" result))
    (setq result (replace-regexp-in-string "\\\\r" "\r" result))
    (setq result (replace-regexp-in-string "\\\\'" "'" result))
    (setq result (replace-regexp-in-string "\\\\\"" "\"" result))
    (setq result (replace-regexp-in-string "\\\\\\\\" "\\" result))
    result))

(defun mcp-server-emacs-tools--escape-normalized-replacer (content find)
  "Find FIND in CONTENT after unescaping escape sequences."
  (let* ((unescaped (mcp-server-emacs-tools--unescape-string find))
         (results '()))
    (when (string-match-p (regexp-quote unescaped) content)
      (push unescaped results))
    (nreverse results)))

(defun mcp-server-emacs-tools--trimmed-boundary-replacer (content find)
  "Find FIND in CONTENT by trying the trimmed version."
  (let ((trimmed (string-trim find)))
    (when (and (not (string= trimmed find))
               (string-match-p (regexp-quote trimmed) content))
      (list trimmed))))

(defun mcp-server-emacs-tools--context-aware-replacer (content find)
  "Find FIND in CONTENT using context-aware block matching."
  (let* ((find-lines (split-string find "\n" nil))
         (find-lines (if (and find-lines (string= (car (last find-lines)) ""))
                         (butlast find-lines) find-lines))
         (flen (length find-lines)))
    (when (< flen 3) (cl-return-from mcp-server-emacs-tools--context-aware-replacer nil))
    (let* ((content-lines (split-string content "\n" nil))
           (clen (length content-lines))
           (first-line (string-trim (car find-lines)))
           (last-line (string-trim (car (last find-lines))))
           (results '()))
      (cl-loop for i from 0 to (1- clen) do
        (when (string= (string-trim (nth i content-lines)) first-line)
          (cl-loop for j from (+ i 2) to (1- clen) do
            (when (string= (string-trim (nth j content-lines)) last-line)
              (let* ((block-lines (cl-subseq content-lines i (1+ j)))
                     (blen (length block-lines)))
                (when (= blen flen)
                  (let ((matching 0) (total 0))
                    (cl-loop for k from 1 to (- blen 2) do
                      (let ((bl (string-trim (nth k block-lines)))
                            (fl (string-trim (nth k find-lines))))
                        (when (or (> (length bl) 0) (> (length fl) 0))
                          (cl-incf total)
                          (when (string= bl fl) (cl-incf matching)))))
                    (when (or (= total 0) (>= (/ (float matching) total) 0.5))
                      (push (mapconcat #'identity block-lines "\n") results))))
                (cl-return))))))
      (nreverse results))))

(defun mcp-server-emacs-tools--multi-occurrence-replacer (content find)
  "Return all occurrences of FIND in CONTENT."
  (let ((results '()) (start 0))
    (while (string-match (regexp-quote find) content start)
      (push find results)
      (setq start (match-end 0)))
    (nreverse results)))

;;; Main replace function

(defun mcp-server-emacs-tools--replace (content old-string new-string &optional replace-all)
  "Replace OLD-STRING with NEW-STRING in CONTENT.
If REPLACE-ALL is non-nil, replace all occurrences.
Returns the new content string or signals an error."
  (when (string= old-string new-string)
    (error "No changes to apply: oldString and newString are identical."))
  (let ((replacers (list #'mcp-server-emacs-tools--simple-replacer
                         #'mcp-server-emacs-tools--line-trimmed-replacer
                         #'mcp-server-emacs-tools--block-anchor-replacer
                         #'mcp-server-emacs-tools--whitespace-normalized-replacer
                         #'mcp-server-emacs-tools--indentation-flexible-replacer
                         #'mcp-server-emacs-tools--escape-normalized-replacer
                         #'mcp-server-emacs-tools--trimmed-boundary-replacer
                         #'mcp-server-emacs-tools--context-aware-replacer
                         #'mcp-server-emacs-tools--multi-occurrence-replacer))
        (found nil)
        (result nil))
    (cl-block replace-block
      (dolist (replacer replacers)
        (let ((candidates (funcall replacer content old-string)))
          (dolist (search candidates)
            (when (string-match-p (regexp-quote search) content)
              (setq found t)
              (if replace-all
                  (progn
                    (setq result (replace-regexp-in-string (regexp-quote search) (regexp-quote new-string) content))
                    ;; Actually use string replacement directly to avoid regexp quoting issues
                    (setq result content)
                    (while (string-match (regexp-quote search) result)
                      (setq result (replace-match new-string t t result)))
                    (cl-return-from replace-block result))
                ;; Single replacement: only if there's exactly one occurrence
                (let* ((first (string-match (regexp-quote search) content))
                       (last (let ((pos first) (last-pos first))
                               (while pos
                                 (setq last-pos pos)
                                 (setq pos (string-match (regexp-quote search) content (1+ pos))))
                               last-pos)))
                  (if (= first last)
                      (progn
                        (setq result (concat (substring content 0 first)
                                             new-string
                                             (substring content (+ first (length search)))))
                        (cl-return-from replace-block result))
                    ;; Multiple matches found via this replacer - continue to next
                    )))))))
      (cond
       ((not found)
        (error "Could not find oldString in the file. It must match exactly, including whitespace, indentation, and line endings."))
       (t
        (error "Found multiple matches for oldString. Provide more surrounding context to make the match unique."))))
    result))

;;; Handler

(defun mcp-server-emacs-tools--edit-handler (args)
  "Handle edit tool invocation with ARGS."
  (condition-case err
      (let* ((filepath (alist-get 'filePath args))
             (old-string (alist-get 'oldString args))
             (new-string (alist-get 'newString args))
             (replace-all (alist-get 'replaceAll args)))
        (unless filepath (error "filePath is required"))
        (when (equal old-string new-string)
          (error "No changes to apply: oldString and newString are identical."))
        (let ((filepath (expand-file-name filepath)))
          ;; Create new file if oldString is empty
          (if (and old-string (string= old-string ""))
              (progn
                (let ((dir (file-name-directory filepath)))
                  (when (and dir (not (file-directory-p dir)))
                    (make-directory dir t)))
                (write-region (or new-string "") nil filepath nil 'silent)
                (format "Edit applied successfully. Created: %s" filepath))
            ;; Edit existing file
            (unless (file-exists-p filepath)
              (error "File %s not found" filepath))
            (let* ((content (with-temp-buffer
                              (insert-file-contents filepath)
                              (buffer-string)))
                   (new-content (mcp-server-emacs-tools--replace
                                 content old-string (or new-string "") replace-all)))
              (write-region new-content nil filepath nil 'silent)
              "Edit applied successfully."))))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "edit"
  :title "Edit File"
  :description "Performs exact string replacements in files.

Usage:
- You must use the `read` tool at least once before editing.
- The edit will FAIL if oldString is not found in the file.
- The edit will FAIL if oldString is found multiple times - provide more context to make it unique.
- Use replaceAll for replacing and renaming strings across the file.
- ALWAYS prefer editing existing files. NEVER write new files unless explicitly required.
- Only use emojis if the user explicitly requests it."
  :input-schema '((type . "object")
                  (properties . ((filePath . ((type . "string")
                                              (description . "The absolute path to the file to modify")))
                                 (oldString . ((type . "string")
                                               (description . "The text to replace")))
                                 (newString . ((type . "string")
                                               (description . "The text to replace it with (must be different from oldString)")))
                                 (replaceAll . ((type . "boolean")
                                                (description . "Replace all occurrences of oldString (default false)")))))
                  (required . ["filePath" "oldString" "newString"]))
  :function #'mcp-server-emacs-tools--edit-handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . t)
                 (idempotentHint . :false)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-edit)

;;; mcp-server-emacs-tools-edit.el ends here
