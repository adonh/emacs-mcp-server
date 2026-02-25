;;; mcp-server-emacs-tools-webfetch.el --- Web Fetch MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for fetching URL content and converting HTML to markdown.
;; Uses Emacs built-in url.el. Mirrors the opencode webfetch.ts tool.

;;; Code:

(require 'mcp-server-tools)
(require 'url)
(require 'url-http)
(require 'cl-lib)

(defconst mcp-server-emacs-tools--webfetch-default-timeout 30
  "Default timeout in seconds.")

(defconst mcp-server-emacs-tools--webfetch-max-timeout 120
  "Maximum allowed timeout in seconds.")

(defconst mcp-server-emacs-tools--webfetch-max-size (* 5 1024 1024)
  "Maximum response size in bytes (5MB).")

(defun mcp-server-emacs-tools--webfetch-html-to-text (html)
  "Convert HTML string to plain text by stripping tags."
  (with-temp-buffer
    (insert html)
    ;; Remove script, style blocks
    (goto-char (point-min))
    (while (re-search-forward "<script[^>]*>\\(?:.\\|\n\\)*?</script>" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "<style[^>]*>\\(?:.\\|\n\\)*?</style>" nil t)
      (replace-match ""))
    ;; Add newlines for block elements
    (goto-char (point-min))
    (while (re-search-forward "</?\\(p\\|div\\|br\\|li\\|h[1-6]\\|tr\\|blockquote\\)[^>]*>" nil t)
      (replace-match "\n"))
    ;; Strip remaining tags
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match ""))
    ;; Decode common HTML entities
    (goto-char (point-min))
    (while (re-search-forward "&amp;" nil t) (replace-match "&"))
    (goto-char (point-min))
    (while (re-search-forward "&lt;" nil t) (replace-match "<"))
    (goto-char (point-min))
    (while (re-search-forward "&gt;" nil t) (replace-match ">"))
    (goto-char (point-min))
    (while (re-search-forward "&quot;" nil t) (replace-match "\""))
    (goto-char (point-min))
    (while (re-search-forward "&nbsp;" nil t) (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward "&#[0-9]+;" nil t)
      (let ((code (string-to-number (substring (match-string 0) 2 -1))))
        (replace-match (string code))))
    ;; Collapse whitespace
    (goto-char (point-min))
    (while (re-search-forward "\n\\{3,\\}" nil t) (replace-match "\n\n"))
    (string-trim (buffer-string))))

(defun mcp-server-emacs-tools--webfetch-html-to-markdown (html)
  "Convert HTML to a simplified markdown representation."
  (with-temp-buffer
    (insert html)
    ;; Remove scripts and styles
    (goto-char (point-min))
    (while (re-search-forward "<\\(?:script\\|style\\|meta\\|link\\)[^>]*>\\(?:.*?\n\\)*?.*?</\\(?:script\\|style\\)>" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "<\\(?:script\\|style\\|meta\\|link\\)[^>]*/>" nil t)
      (replace-match ""))
    ;; Headings
    (cl-loop for i from 6 downto 1 do
      (let ((tag (format "h%d" i))
            (hashes (make-string i ?#)))
        (goto-char (point-min))
        (while (re-search-forward (format "<%s[^>]*>\\(.*?\\)</%s>" tag tag) nil t)
          (replace-match (format "%s %s" hashes (match-string 1))))))
    ;; Bold/italic
    (goto-char (point-min))
    (while (re-search-forward "<strong[^>]*>\\(.*?\\)</strong>" nil t)
      (replace-match "**\\1**"))
    (goto-char (point-min))
    (while (re-search-forward "<b[^>]*>\\(.*?\\)</b>" nil t)
      (replace-match "**\\1**"))
    (goto-char (point-min))
    (while (re-search-forward "<em[^>]*>\\(.*?\\)</em>" nil t)
      (replace-match "*\\1*"))
    ;; Code
    (goto-char (point-min))
    (while (re-search-forward "<code[^>]*>\\(.*?\\)</code>" nil t)
      (replace-match "`\\1`"))
    ;; Links
    (goto-char (point-min))
    (while (re-search-forward "<a[^>]+href=\"\\([^\"]+\\)\"[^>]*>\\(.*?\\)</a>" nil t)
      (replace-match "[\\2](\\1)"))
    ;; Block elements with newlines
    (goto-char (point-min))
    (while (re-search-forward "<li[^>]*>\\(.*?\\)</li>" nil t)
      (replace-match "- \\1"))
    (goto-char (point-min))
    (while (re-search-forward "</?\\(p\\|div\\|br\\|ul\\|ol\\|tr\\|blockquote\\|section\\|article\\|header\\|footer\\|main\\)[^>]*>" nil t)
      (replace-match "\n"))
    ;; Strip remaining tags
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t) (replace-match ""))
    ;; Decode entities
    (goto-char (point-min))
    (while (re-search-forward "&amp;" nil t) (replace-match "&"))
    (goto-char (point-min))
    (while (re-search-forward "&lt;" nil t) (replace-match "<"))
    (goto-char (point-min))
    (while (re-search-forward "&gt;" nil t) (replace-match ">"))
    (goto-char (point-min))
    (while (re-search-forward "&quot;" nil t) (replace-match "\""))
    (goto-char (point-min))
    (while (re-search-forward "&nbsp;" nil t) (replace-match " "))
    ;; Collapse whitespace
    (goto-char (point-min))
    (while (re-search-forward "\n\\{3,\\}" nil t) (replace-match "\n\n"))
    (string-trim (buffer-string))))

(defun mcp-server-emacs-tools--webfetch-fetch (url timeout-secs)
  "Fetch URL with TIMEOUT-SECS timeout.
Returns (status content-type body) or signals an error."
  (let* ((url-request-extra-headers
          `(("User-Agent" . "Mozilla/5.0 (compatible; Emacs MCP)")
            ("Accept" . "text/html,application/xhtml+xml,text/plain,*/*")))
         (url-retrieve-number-of-calls 0)
         result)
    ;; Use synchronous fetch with timeout via with-timeout
    (with-timeout (timeout-secs (error "Request timed out after %d seconds" timeout-secs))
      (with-current-buffer (url-retrieve-synchronously url t nil timeout-secs)
        (goto-char (point-min))
        (let* ((status (progn
                         (re-search-forward "HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                         (string-to-number (or (match-string 1) "0"))))
               (content-type (progn
                               (re-search-forward "Content-Type: \\([^\r\n]+\\)" nil t)
                               (or (match-string 1) "text/plain")))
               ;; Skip headers - find blank line
               (_ (re-search-forward "\r\n\r\n\\|\n\n" nil t))
               (body (buffer-substring-no-properties (point) (point-max))))
          (kill-buffer)
          (when (>= status 400)
            (error "Request failed with status code: %d" status))
          (list status content-type body))))))

(defun mcp-server-emacs-tools--webfetch-handler (args)
  "Handle webfetch tool invocation with ARGS."
  (condition-case err
      (let* ((url (alist-get 'url args))
             (format-type (or (alist-get 'format args) "markdown"))
             (timeout (alist-get 'timeout args)))
        (unless url (error "url is required"))
        (unless (string-match-p "^https?://" url)
          (error "URL must start with http:// or https://"))
        (let* ((timeout-secs (min (or timeout mcp-server-emacs-tools--webfetch-default-timeout)
                                   mcp-server-emacs-tools--webfetch-max-timeout))
               (result (mcp-server-emacs-tools--webfetch-fetch url timeout-secs))
               (content-type (nth 1 result))
               (body (nth 2 result))
               (is-html (string-match-p "text/html" content-type)))
          (cond
           ((string= format-type "markdown")
            (if is-html
                (mcp-server-emacs-tools--webfetch-html-to-markdown body)
              body))
           ((string= format-type "text")
            (if is-html
                (mcp-server-emacs-tools--webfetch-html-to-text body)
              body))
           ((string= format-type "html")
            body)
           (t body))))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "webfetch"
  :title "Web Fetch"
  :description "Fetch content from a specified URL and return its contents in a readable format.

Usage notes:
- The URL must be a fully-formed, valid URL.
- This tool is read-only and will not work for requests intended to have side effects.
- Format options: \"markdown\" (default), \"text\", or \"html\"
- HTTP URLs are supported (no automatic HTTPS upgrade in Emacs)."
  :input-schema '((type . "object")
                  (properties . ((url . ((type . "string")
                                         (description . "The URL to fetch content from")))
                                 (format . ((type . "string")
                                            (enum . ("text" "markdown" "html"))
                                            (description . "The format to return the content in. Defaults to markdown.")))
                                 (timeout . ((type . "number")
                                             (description . "Optional timeout in seconds (max 120)")))))
                  (required . ["url"]))
  :function #'mcp-server-emacs-tools--webfetch-handler
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false)
                 (idempotentHint . t)
                 (openWorldHint . t))))

(provide 'mcp-server-emacs-tools-webfetch)

;;; mcp-server-emacs-tools-webfetch.el ends here
