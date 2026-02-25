;;; mcp-server-emacs-tools-websearch.el --- Web Search MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for web search via Exa AI API.
;; Mirrors the opencode websearch.ts tool.
;; Requires network access and the Exa MCP API.

;;; Code:

(require 'mcp-server-tools)
(require 'url)
(require 'json)
(require 'cl-lib)

(defconst mcp-server-emacs-tools--websearch-api-url "https://mcp.exa.ai/mcp"
  "Exa MCP API endpoint.")

(defconst mcp-server-emacs-tools--websearch-default-results 8
  "Default number of search results.")

(defun mcp-server-emacs-tools--websearch-post (url body timeout-secs)
  "POST JSON BODY to URL with TIMEOUT-SECS. Returns response body string."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Accept" . "application/json, text/event-stream")))
         (url-request-data (encode-coding-string body 'utf-8)))
    (with-timeout (timeout-secs (error "Search request timed out"))
      (with-current-buffer (url-retrieve-synchronously url t nil timeout-secs)
        (goto-char (point-min))
        (re-search-forward "\r\n\r\n\\|\n\n" nil t)
        (let ((response (buffer-substring-no-properties (point) (point-max))))
          (kill-buffer)
          response)))))

(defun mcp-server-emacs-tools--websearch-parse-sse (text)
  "Parse SSE (Server-Sent Events) response TEXT and return first data JSON."
  (let ((result nil))
    (dolist (line (split-string text "\n" t))
      (when (and (not result) (string-prefix-p "data: " line))
        (condition-case nil
            (setq result (json-read-from-string (substring line 6)))
          (error nil))))
    result))

(defun mcp-server-emacs-tools--websearch-handler (args)
  "Handle websearch tool invocation with ARGS."
  (condition-case err
      (let* ((query (alist-get 'query args))
             (num-results (or (alist-get 'numResults args)
                               mcp-server-emacs-tools--websearch-default-results))
             (livecrawl (or (alist-get 'livecrawl args) "fallback"))
             (type (or (alist-get 'type args) "auto"))
             (context-max (alist-get 'contextMaxCharacters args)))
        (unless query (error "query is required"))
        (let* ((request `((jsonrpc . "2.0")
                           (id . 1)
                           (method . "tools/call")
                           (params . ((name . "web_search_exa")
                                      (arguments . ,(append
                                                     `((query . ,query)
                                                       (type . ,type)
                                                       (numResults . ,num-results)
                                                       (livecrawl . ,livecrawl))
                                                     (when context-max
                                                       `((contextMaxCharacters . ,context-max)))))))))
               (body (json-encode request))
               (response (mcp-server-emacs-tools--websearch-post
                          mcp-server-emacs-tools--websearch-api-url body 25))
               (parsed (mcp-server-emacs-tools--websearch-parse-sse response)))
          (if (and parsed
                   (alist-get 'result parsed)
                   (alist-get 'content (alist-get 'result parsed)))
              (let ((content (alist-get 'content (alist-get 'result parsed))))
                (if (and (vectorp content) (> (length content) 0))
                    (alist-get 'text (aref content 0))
                  "No search results found."))
            "No search results found. Please try a different query.")))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "websearch"
  :title "Web Search"
  :description (format "Search the web using Exa AI - performs real-time web searches and can scrape content from specific URLs.
- Provides up-to-date information for current events and recent data
- Supports configurable result counts
- Use this tool for accessing information beyond knowledge cutoff

The current year is %s. Use this year when searching for recent information."
                        (format-time-string "%Y"))
  :input-schema '((type . "object")
                  (properties . ((query . ((type . "string")
                                           (description . "Websearch query")))
                                 (numResults . ((type . "number")
                                                (description . "Number of search results to return (default: 8)")))
                                 (livecrawl . ((type . "string")
                                               (enum . ("fallback" "preferred"))
                                               (description . "Live crawl mode")))
                                 (type . ((type . "string")
                                          (enum . ("auto" "fast" "deep"))
                                          (description . "Search type - auto (default), fast, or deep")))
                                 (contextMaxCharacters . ((type . "number")
                                                          (description . "Maximum characters for context (default: 10000)")))))
                  (required . ["query"]))
  :function #'mcp-server-emacs-tools--websearch-handler
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false)
                 (idempotentHint . :false)
                 (openWorldHint . t))))

(provide 'mcp-server-emacs-tools-websearch)

;;; mcp-server-emacs-tools-websearch.el ends here
