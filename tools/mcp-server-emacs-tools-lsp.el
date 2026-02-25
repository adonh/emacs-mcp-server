;;; mcp-server-emacs-tools-lsp.el --- LSP Operations MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for Language Server Protocol operations.
;; Mirrors the opencode lsp.ts tool.
;; Works with both eglot (built-in) and lsp-mode.
;; Falls back gracefully if no LSP server is running for the file.

;;; Code:

(require 'mcp-server-tools)
(require 'json)
(require 'cl-lib)

(declare-function eglot--current-server-or-lose "eglot")
(declare-function eglot--TextDocumentPositionParams "eglot")
(declare-function eglot-managed-p "eglot")
(declare-function jsonrpc-request "jsonrpc")

(defconst mcp-server-emacs-tools--lsp-operations
  '("goToDefinition" "findReferences" "hover" "documentSymbol"
    "workspaceSymbol" "goToImplementation" "prepareCallHierarchy"
    "incomingCalls" "outgoingCalls")
  "Supported LSP operations.")

(defun mcp-server-emacs-tools--lsp-eglot-available-p (filepath)
  "Return non-nil if eglot is managing a buffer for FILEPATH."
  (and (fboundp 'eglot-managed-p)
       (let ((buf (find-buffer-visiting filepath)))
         (and buf (with-current-buffer buf (eglot-managed-p))))))

(defun mcp-server-emacs-tools--lsp-open-file (filepath)
  "Ensure FILEPATH is visited in a buffer. Returns the buffer."
  (or (find-buffer-visiting filepath)
      (find-file-noselect filepath)))

(defun mcp-server-emacs-tools--lsp-make-position (line character)
  "Make an LSP position with LINE and CHARACTER (0-based)."
  `((line . ,line) (character . ,character)))

(defun mcp-server-emacs-tools--lsp-eglot-request (filepath operation line character)
  "Perform an LSP OPERATION via eglot on FILEPATH at LINE:CHARACTER (1-based).
Returns result as a JSON string."
  (let* ((buf (mcp-server-emacs-tools--lsp-open-file filepath))
         (line0 (1- line))
         (char0 (1- character)))
    (with-current-buffer buf
      (let ((server (eglot--current-server-or-lose)))
        (let ((result
               (pcase operation
                 ("goToDefinition"
                  (jsonrpc-request server :textDocument/definition
                                   (eglot--TextDocumentPositionParams buf (point))))
                 ("findReferences"
                  (jsonrpc-request server :textDocument/references
                                   `(:textDocument (:uri ,(eglot--TextDocumentIdentifier))
                                     :position (:line ,line0 :character ,char0)
                                     :context (:includeDeclaration t))))
                 ("hover"
                  (jsonrpc-request server :textDocument/hover
                                   `(:textDocument (:uri ,(buffer-file-name))
                                     :position (:line ,line0 :character ,char0))))
                 ("documentSymbol"
                  (jsonrpc-request server :textDocument/documentSymbol
                                   `(:textDocument (:uri ,(buffer-file-name)))))
                 ("workspaceSymbol"
                  (jsonrpc-request server :workspace/symbol
                                   `(:query "")))
                 ("goToImplementation"
                  (jsonrpc-request server :textDocument/implementation
                                   `(:textDocument (:uri ,(buffer-file-name))
                                     :position (:line ,line0 :character ,char0))))
                 (_
                  (error "Operation %s not yet implemented via eglot" operation)))))
          (json-encode result))))))

(defun mcp-server-emacs-tools--lsp-handler (args)
  "Handle lsp tool invocation with ARGS."
  (condition-case err
      (let* ((operation (alist-get 'operation args))
             (filepath (alist-get 'filePath args))
             (line (alist-get 'line args))
             (character (alist-get 'character args)))
        (unless operation (error "operation is required"))
        (unless filepath (error "filePath is required"))
        (unless line (error "line is required"))
        (unless character (error "character is required"))
        (unless (member operation mcp-server-emacs-tools--lsp-operations)
          (error "Unknown operation: %s. Supported: %s"
                 operation
                 (mapconcat #'identity mcp-server-emacs-tools--lsp-operations ", ")))
        (let ((filepath (expand-file-name filepath)))
          (unless (file-exists-p filepath)
            (error "File not found: %s" filepath))
          (unless (mcp-server-emacs-tools--lsp-eglot-available-p filepath)
            (error "No LSP server available for this file type. Ensure eglot is running for: %s" filepath))
          (mcp-server-emacs-tools--lsp-eglot-request filepath operation line character)))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "lsp"
  :title "LSP Operations"
  :description "Interact with Language Server Protocol (LSP) servers to get code intelligence features.

Supported operations:
- goToDefinition: Find where a symbol is defined
- findReferences: Find all references to a symbol
- hover: Get hover information (documentation, type info) for a symbol
- documentSymbol: Get all symbols (functions, classes, variables) in a document
- workspaceSymbol: Search for symbols across the entire workspace
- goToImplementation: Find implementations of an interface or abstract method
- prepareCallHierarchy: Get call hierarchy item at a position
- incomingCalls: Find all functions/methods that call the function at a position
- outgoingCalls: Find all functions/methods called by the function at a position

All operations require:
- filePath: The file to operate on
- line: The line number (1-based, as shown in editors)
- character: The character offset (1-based, as shown in editors)

Note: Requires eglot or lsp-mode to be active for the file type."
  :input-schema '((type . "object")
                  (properties . ((operation . ((type . "string")
                                               (enum . ("goToDefinition" "findReferences" "hover"
                                                        "documentSymbol" "workspaceSymbol"
                                                        "goToImplementation" "prepareCallHierarchy"
                                                        "incomingCalls" "outgoingCalls"))
                                               (description . "The LSP operation to perform")))
                                 (filePath . ((type . "string")
                                              (description . "The absolute or relative path to the file")))
                                 (line . ((type . "number")
                                          (description . "The line number (1-based, as shown in editors)")))
                                 (character . ((type . "number")
                                               (description . "The character offset (1-based, as shown in editors)")))))
                  (required . ["operation" "filePath" "line" "character"]))
  :function #'mcp-server-emacs-tools--lsp-handler
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false)
                 (idempotentHint . t)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-lsp)

;;; mcp-server-emacs-tools-lsp.el ends here
