;;; mcp-server-emacs-tools-codesearch.el --- Code Search MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for searching code documentation via Exa AI API.
;; Mirrors the opencode codesearch.ts tool.

;;; Code:

(require 'mcp-server-tools)
(require 'url)
(require 'json)
(require 'cl-lib)

(defconst mcp-server-emacs-tools--codesearch-api-url "https://mcp.exa.ai/mcp"
  "Exa MCP API endpoint.")

(defun mcp-server-emacs-tools--codesearch-post (url body timeout-secs)
  "POST JSON BODY to URL with TIMEOUT-SECS. Returns response body string."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Accept" . "application/json, text/event-stream")))
         (url-request-data (encode-coding-string body 'utf-8)))
    (with-timeout (timeout-secs (error "Code search request timed out"))
      (with-current-buffer (url-retrieve-synchronously url t nil timeout-secs)
        (goto-char (point-min))
        (re-search-forward "\r\n\r\n\\|\n\n" nil t)
        (let ((response (buffer-substring-no-properties (point) (point-max))))
          (kill-buffer)
          response)))))

(defun mcp-server-emacs-tools--codesearch-parse-sse (text)
  "Parse SSE response TEXT and return first data JSON."
  (let ((result nil))
    (dolist (line (split-string text "\n" t))
      (when (and (not result) (string-prefix-p "data: " line))
        (condition-case nil
            (setq result (json-read-from-string (substring line 6)))
          (error nil))))
    result))

(defun mcp-server-emacs-tools--codesearch-handler (args)
  "Handle codesearch tool invocation with ARGS."
  (condition-case err
      (let* ((query (alist-get 'query args))
             (tokens-num (or (alist-get 'tokensNum args) 5000)))
        (unless query (error "query is required"))
        (when (< tokens-num 1000)
          (error "tokensNum must be at least 1000"))
        (when (> tokens-num 50000)
          (error "tokensNum must be at most 50000"))
        (let* ((request `((jsonrpc . "2.0")
                           (id . 1)
                           (method . "tools/call")
                           (params . ((name . "get_code_context_exa")
                                      (arguments . ((query . ,query)
                                                    (tokensNum . ,tokens-num)))))))
               (body (json-encode request))
               (response (mcp-server-emacs-tools--codesearch-post
                          mcp-server-emacs-tools--codesearch-api-url body 30))
               (parsed (mcp-server-emacs-tools--codesearch-parse-sse response)))
          (if (and parsed
                   (alist-get 'result parsed)
                   (alist-get 'content (alist-get 'result parsed)))
              (let ((content (alist-get 'content (alist-get 'result parsed))))
                (if (and (vectorp content) (> (length content) 0))
                    (alist-get 'text (aref content 0))
                  "No code snippets or documentation found."))
            "No code snippets or documentation found. Please try a different query, be more specific about the library or programming concept, or check the spelling of framework names.")))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "codesearch"
  :title "Code Search"
  :description "Search and get relevant context for any programming task using Exa Code API.
- Provides the highest quality and freshest context for libraries, SDKs, and APIs
- Use this tool for ANY question or task related to programming
- Returns comprehensive code examples, documentation, and API references

Usage notes:
- Adjustable token count (1000-50000) for focused or comprehensive results
- Default 5000 tokens provides balanced context for most queries
- Examples: 'React useState hook examples', 'Python pandas dataframe filtering'"
  :input-schema '((type . "object")
                  (properties . ((query . ((type . "string")
                                           (description . "Search query to find relevant context for APIs, Libraries, and SDKs.")))
                                 (tokensNum . ((type . "number")
                                               (description . "Number of tokens to return (1000-50000). Default is 5000.")))))
                  (required . ["query"]))
  :function #'mcp-server-emacs-tools--codesearch-handler
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false)
                 (idempotentHint . :false)
                 (openWorldHint . t))))

(provide 'mcp-server-emacs-tools-codesearch)

;;; mcp-server-emacs-tools-codesearch.el ends here
