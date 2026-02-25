;;; mcp-server-emacs-tools-batch.el --- Batch Tool Execution MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for executing multiple tool calls sequentially (Emacs is single-threaded).
;; Mirrors the opencode batch.ts tool.
;; Note: Unlike the TypeScript version, execution is sequential (not parallel)
;; since Emacs doesn't have true parallelism for Lisp code.

;;; Code:

(require 'mcp-server-tools)
(require 'json)
(require 'cl-lib)

(defconst mcp-server-emacs-tools--batch-max-calls 25
  "Maximum number of tool calls allowed per batch.")

(defconst mcp-server-emacs-tools--batch-disallowed '("batch")
  "Tools that cannot be used within batch.")

(defun mcp-server-emacs-tools--batch-handler (args)
  "Handle batch tool invocation with ARGS."
  (condition-case err
      (let* ((tool-calls-raw (alist-get 'tool_calls args))
             (tool-calls (if (vectorp tool-calls-raw) (append tool-calls-raw nil) tool-calls-raw)))
        (unless tool-calls (error "tool_calls is required and must be non-empty"))
        (let* ((limited (cl-subseq tool-calls 0
                                    (min (length tool-calls)
                                         mcp-server-emacs-tools--batch-max-calls)))
               (discarded (cl-subseq tool-calls (length limited)))
               (results '())
               (success-count 0)
               (fail-count 0))
          ;; Execute each tool call
          (dolist (call limited)
            (let* ((tool-name (alist-get 'tool call))
                   (params (alist-get 'parameters call))
                   (result
                    (condition-case call-err
                        (progn
                          (when (member tool-name mcp-server-emacs-tools--batch-disallowed)
                            (error "Tool '%s' is not allowed in batch" tool-name))
                          (let ((tool (mcp-server-tools-get tool-name)))
                            (unless tool
                              (error "Tool '%s' not found" tool-name))
                            (let* ((output (mcp-server-tools-call tool-name params))
                                   (text (if (vectorp output)
                                             (mapconcat (lambda (item)
                                                          (alist-get 'text item ""))
                                                        (append output nil) "\n")
                                           (format "%S" output))))
                              (cl-incf success-count)
                              `((tool . ,tool-name) (success . t) (output . ,text)))))
                      (error
                       (cl-incf fail-count)
                       `((tool . ,tool-name) (success . :false)
                         (error . ,(error-message-string call-err)))))))
            (push result results)))
          ;; Account for discarded calls
          (dolist (call discarded)
            (cl-incf fail-count)
            (push `((tool . ,(alist-get 'tool call))
                    (success . :false)
                    (error . "Maximum of 25 tools allowed in batch"))
                  results))
          (let* ((results (nreverse results))
                 (total (length results))
                 (summary (if (> fail-count 0)
                              (format "Executed %d/%d tools successfully. %d failed."
                                      success-count total fail-count)
                            (format "All %d tools executed successfully." success-count)))
                 (detail (json-encode (vconcat results))))
            (format "%s\n\nResults:\n%s" summary detail))))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "batch"
  :title "Batch Tool Execution"
  :description "Executes multiple tool calls sequentially to reduce round-trips.

Note: In Emacs, execution is sequential (not parallel) due to single-threaded nature.

Payload Format (JSON array):
[{\"tool\": \"read\", \"parameters\": {\"filePath\": \"src/index.el\"}},{\"tool\": \"grep\", \"parameters\": {\"pattern\": \"defun\"}}]

Notes:
- 1-25 tool calls per batch
- Partial failures do not stop other tool calls
- Do NOT use batch within another batch

Good Use Cases:
- Read many files
- grep + glob + read combos
- Multi-part edits on the same or different files"
  :input-schema '((type . "object")
                  (properties . ((tool_calls . ((type . "array")
                                                (items . ((type . "object")
                                                          (properties . ((tool . ((type . "string")
                                                                                  (description . "The name of the tool to execute")))
                                                                         (parameters . ((type . "object")
                                                                                        (description . "Parameters for the tool")))))
                                                          (required . ["tool" "parameters"])))
                                                (description . "Array of tool calls to execute")))))
                  (required . ["tool_calls"]))
  :function #'mcp-server-emacs-tools--batch-handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . t)
                 (idempotentHint . :false)
                 (openWorldHint . t))))

(provide 'mcp-server-emacs-tools-batch)

;;; mcp-server-emacs-tools-batch.el ends here
