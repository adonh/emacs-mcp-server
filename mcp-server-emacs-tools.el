;;; mcp-server-emacs-tools.el --- Emacs-specific MCP Tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; This module loads Emacs-specific MCP tools from the tools/ directory.
;; Tools self-register on require.  Use `mcp-server-emacs-tools-enabled'
;; to control which tools are exposed to LLM clients at runtime.

;;; Code:

(require 'mcp-server-tools)

(defgroup mcp-server-emacs-tools nil
  "Emacs-specific MCP tools configuration."
  :group 'mcp-server
  :prefix "mcp-server-emacs-tools-")

(defcustom mcp-server-emacs-tools-enabled 'all
  "Which MCP tools to enable.
Can be `all' to enable all available tools, or a list of tool
names (symbols) to enable selectively.

Original tools:
- `eval-elisp' - Execute arbitrary Elisp expressions
- `get-diagnostics' - Get flycheck/flymake diagnostics

Agentic tools (translated from opencode):
- `read' - Read files and directories with line numbers
- `write' - Write/create files
- `edit' - String replacement in files with fuzzy matching
- `multiedit' - Multiple sequential edits to a single file
- `grep' - Regex content search (ripgrep or grep fallback)
- `glob' - File pattern matching
- `list' - Directory tree listing
- `bash' - Shell command execution with timeout
- `webfetch' - Fetch URL content (HTML to markdown)
- `websearch' - Web search via Exa AI API
- `codesearch' - Code documentation search via Exa AI API
- `todowrite' - Write/update session todo list
- `todoread' - Read current session todo list
- `question' - Ask user questions with options
- `apply_patch' - Apply multi-file patches
- `lsp' - LSP operations (requires eglot)
- `batch' - Execute multiple tools sequentially
- `task' - Launch subtasks/subagent sessions
- `skill' - Load specialized skill instructions

Example: \\='(read write edit grep) to enable only core file tools.

Changes take effect immediately - disabled tools are hidden from
LLM clients and cannot be called."
  :type '(choice (const :tag "All tools" all)
                 (repeat :tag "Selected tools" symbol))
  :group 'mcp-server-emacs-tools)

(defconst mcp-server-emacs-tools--available
  '(;; Original tools
    ;; @@@ adonhwang: 2026-02-24: (eval-elisp . mcp-server-emacs-tools-eval-elisp)
    (get-diagnostics . mcp-server-emacs-tools-diagnostics)
    ;; Agentic file operation tools
    (read . mcp-server-emacs-tools-read)
    (write . mcp-server-emacs-tools-write)
    (edit . mcp-server-emacs-tools-edit)
    (multiedit . mcp-server-emacs-tools-multiedit)
    ;; Agentic search tools
    (grep . mcp-server-emacs-tools-grep)
    (glob . mcp-server-emacs-tools-glob)
    (list . mcp-server-emacs-tools-ls)
    ;; Agentic shell tool
    (bash . mcp-server-emacs-tools-bash)
    ;; Agentic web tools
    (webfetch . mcp-server-emacs-tools-webfetch)
    (websearch . mcp-server-emacs-tools-websearch)
    (codesearch . mcp-server-emacs-tools-codesearch)
    ;; Agentic session management tools
    (todowrite . mcp-server-emacs-tools-todo)
    (todoread . mcp-server-emacs-tools-todo)
    (question . mcp-server-emacs-tools-question)
    ;; Agentic advanced tools
    (apply_patch . mcp-server-emacs-tools-apply-patch)
    (lsp . mcp-server-emacs-tools-lsp)
    ;; Agentic meta tools
    (batch . mcp-server-emacs-tools-batch)
    (task . mcp-server-emacs-tools-task)
    (skill . mcp-server-emacs-tools-skill))
  "Alist mapping tool names (symbols) to their feature names.")

;; Add tools directory to load path
(let* ((this-file (or load-file-name buffer-file-name))
       (tools-dir (and this-file
                       (expand-file-name "tools" (file-name-directory this-file)))))
  (when tools-dir
    (add-to-list 'load-path tools-dir)))

(defun mcp-server-emacs-tools--tool-enabled-p (tool-name)
  "Return non-nil if TOOL-NAME is enabled.
TOOL-NAME can be a string or symbol."
  (let ((name-sym (if (stringp tool-name) (intern tool-name) tool-name)))
    (or (eq mcp-server-emacs-tools-enabled 'all)
        (memq name-sym mcp-server-emacs-tools-enabled))))

;; Set up the filter for mcp-server-tools
(setq mcp-server-tools-filter #'mcp-server-emacs-tools--tool-enabled-p)

;; Load all tool modules (they self-register).
;; Use a set to avoid loading the same feature twice (e.g. todo registers two tools).
(let ((loaded '()))
  (dolist (tool-spec mcp-server-emacs-tools--available)
    (let ((feature (cdr tool-spec)))
      (unless (memq feature loaded)
        (require feature)
        (push feature loaded)))))

(provide 'mcp-server-emacs-tools)

;;; mcp-server-emacs-tools.el ends here
