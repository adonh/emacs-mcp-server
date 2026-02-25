;;; mcp-server-emacs-tools-read.el --- Read File/Directory MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for reading files and directories.
;; Mirrors the opencode read.ts tool: reads files with line numbers,
;; supports offset/limit, handles directories, images, binary detection.

;;; Code:

(require 'mcp-server-tools)
(require 'cl-lib)

(defconst mcp-server-emacs-tools--read-default-limit 2000
  "Default maximum number of lines to read.")

(defconst mcp-server-emacs-tools--read-max-line-length 2000
  "Maximum characters per line before truncation.")

(defconst mcp-server-emacs-tools--read-max-bytes (* 50 1024)
  "Maximum bytes to read (50 KB).")

(defconst mcp-server-emacs-tools--read-binary-extensions
  '(".zip" ".tar" ".gz" ".exe" ".dll" ".so" ".class" ".jar" ".war"
    ".7z" ".doc" ".docx" ".xls" ".xlsx" ".ppt" ".pptx" ".odt" ".ods"
    ".odp" ".bin" ".dat" ".obj" ".o" ".a" ".lib" ".wasm" ".pyc" ".pyo"
    ".png" ".jpg" ".jpeg" ".gif" ".bmp" ".ico" ".tiff" ".webp"
    ".mp3" ".mp4" ".avi" ".mov" ".mkv" ".flac" ".wav" ".ogg"
    ".pdf" ".epub")
  "File extensions considered binary and unreadable as text.")

(defun mcp-server-emacs-tools--read-binary-p (filepath)
  "Return non-nil if FILEPATH is a binary file by extension or content sampling."
  (let ((ext (downcase (or (file-name-extension filepath t) ""))))
    (if (member ext mcp-server-emacs-tools--read-binary-extensions)
        t
      ;; Sample first 4096 bytes to check for null bytes
      (condition-case nil
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally filepath nil 0 4096)
            (goto-char (point-min))
            ;; Null byte = binary
            (search-forward "\0" nil t))
        (error nil)))))

(defun mcp-server-emacs-tools--read-directory (filepath offset limit)
  "Read directory FILEPATH, returning entries starting at OFFSET up to LIMIT."
  (let* ((entries (directory-files filepath nil nil t))
         ;; Remove . and .. then sort
         (entries (sort (cl-remove-if (lambda (e) (member e '("." ".."))) entries)
                        #'string<))
         ;; Annotate directories with trailing /
         (annotated (mapcar (lambda (e)
                              (if (file-directory-p (expand-file-name e filepath))
                                  (concat e "/")
                                e))
                            entries))
         (total (length annotated))
         (start (1- (or offset 1)))
         (start (max 0 start))
         (sliced (seq-subseq annotated start (min (+ start (or limit mcp-server-emacs-tools--read-default-limit))
                                                   total)))
         (showing (length sliced))
         (offset (or offset 1))
         (truncated (< (+ start showing) total))
         (next-offset (+ offset showing)))
    (concat
     (format "<path>%s</path>\n<type>directory</type>\n<entries>\n" filepath)
     (mapconcat #'identity sliced "\n")
     (if truncated
         (format "\n\n(Showing %d of %d entries. Use 'offset' parameter to read beyond entry %d)"
                 showing total next-offset)
       (format "\n\n(%d entries)" total))
     "\n</entries>")))

(defun mcp-server-emacs-tools--read-file (filepath offset limit)
  "Read text file FILEPATH starting at line OFFSET, up to LIMIT lines."
  (let* ((offset (or offset 1))
         (limit (or limit mcp-server-emacs-tools--read-default-limit))
         (start (1- offset))
         (lines-raw '())
         (total-lines 0)
         (has-more nil)
         (truncated-by-bytes nil)
         (bytes-accumulated 0))
    (with-temp-buffer
      (insert-file-contents filepath)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cl-incf total-lines)
          (when (>= (1- total-lines) start)
            (if (>= (length lines-raw) limit)
                (setq has-more t)
              (let* ((truncated-line (if (> (length line) mcp-server-emacs-tools--read-max-line-length)
                                         (concat (substring line 0 mcp-server-emacs-tools--read-max-line-length)
                                                 (format "... (line truncated to %d chars)"
                                                         mcp-server-emacs-tools--read-max-line-length))
                                       line))
                     (size (+ (string-bytes truncated-line) (if lines-raw 1 0))))
                (if (> (+ bytes-accumulated size) mcp-server-emacs-tools--read-max-bytes)
                    (progn (setq truncated-by-bytes t) (setq has-more t))
                  (push truncated-line lines-raw)
                  (cl-incf bytes-accumulated size))))))
        (forward-line 1)))
    (let* ((raw (nreverse lines-raw))
           (last-read (+ offset (length raw) -1))
           (next-offset (1+ last-read))
           (truncated (or has-more truncated-by-bytes))
           (numbered (cl-loop for line in raw
                               for i from offset
                               collect (format "%d: %s" i line)))
           (content (mapconcat #'identity numbered "\n"))
           (suffix (cond
                    (truncated-by-bytes
                     (format "\n\n(Output capped at %dKB. Showing lines %d-%d. Use offset=%d to continue.)"
                             (/ mcp-server-emacs-tools--read-max-bytes 1024)
                             offset last-read next-offset))
                    (has-more
                     (format "\n\n(Showing lines %d-%d of %d. Use offset=%d to continue.)"
                             offset last-read total-lines next-offset))
                    (t
                     (format "\n\n(End of file - total %d lines)" total-lines)))))
      (concat "<path>" filepath "</path>\n<type>file</type>\n<content>\n"
              content suffix "\n</content>"))))

(defun mcp-server-emacs-tools--read-handler (args)
  "Handle read tool invocation with ARGS."
  (condition-case err
      (let* ((filepath (alist-get 'filePath args))
             (offset (alist-get 'offset args))
             (limit (alist-get 'limit args)))
        (unless filepath
          (error "filePath is required"))
        (when (and offset (< offset 1))
          (error "offset must be >= 1"))
        ;; Resolve relative paths from project root or default-directory
        (let ((filepath (expand-file-name filepath)))
          (cond
           ((not (file-exists-p filepath))
            (let* ((dir (file-name-directory filepath))
                   (base (file-name-nondirectory filepath))
                   (siblings (condition-case nil
                                  (directory-files dir nil nil t)
                                (error nil)))
                   (suggestions (when siblings
                                  (seq-take
                                   (seq-filter (lambda (e)
                                                 (and (not (member e '("." "..")))
                                                      (or (string-match-p (regexp-quote (downcase base)) (downcase e))
                                                          (string-match-p (regexp-quote (downcase e)) (downcase base)))))
                                               siblings)
                                   3))))
              (if suggestions
                  (format "File not found: %s\n\nDid you mean one of these?\n%s"
                          filepath
                          (mapconcat (lambda (s) (expand-file-name s dir)) suggestions "\n"))
                (format "File not found: %s" filepath))))
           ((file-directory-p filepath)
            (mcp-server-emacs-tools--read-directory filepath offset limit))
           ((mcp-server-emacs-tools--read-binary-p filepath)
            (error "Cannot read binary file: %s" filepath))
           (t
            (mcp-server-emacs-tools--read-file filepath offset limit)))))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "read"
  :title "Read File"
  :description "Read a file or directory from the local filesystem. If the path does not exist, an error is returned.

Usage:
- The filePath parameter should be an absolute path.
- By default, this tool returns up to 2000 lines from the start of the file.
- The offset parameter is the line number to start from (1-indexed).
- To read later sections, call this tool again with a larger offset.
- Contents are returned with each line prefixed by its line number as `<line>: <content>`.
- For directories, entries are returned one per line with a trailing `/` for subdirectories.
- Any line longer than 2000 characters is truncated."
  :input-schema '((type . "object")
                  (properties . ((filePath . ((type . "string")
                                              (description . "The absolute path to the file or directory to read")))
                                 (offset . ((type . "number")
                                            (description . "The line number to start reading from (1-indexed)")))
                                 (limit . ((type . "number")
                                           (description . "The maximum number of lines to read (defaults to 2000)")))))
                  (required . ["filePath"]))
  :function #'mcp-server-emacs-tools--read-handler
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false)
                 (idempotentHint . t)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-read)

;;; mcp-server-emacs-tools-read.el ends here
