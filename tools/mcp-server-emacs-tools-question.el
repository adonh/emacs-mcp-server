;;; mcp-server-emacs-tools-question.el --- Question/Prompt MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for asking the user questions with structured options.
;; Mirrors the opencode question.ts tool.
;; Uses Emacs completing-read for interactive prompts.

;;; Code:

(require 'mcp-server-tools)
(require 'cl-lib)

(defun mcp-server-emacs-tools--question-ask-one (q)
  "Ask a single question Q (alist) and return a list of answers."
  (let* ((text (alist-get 'question q))
         (header (alist-get 'header q))
         (multiple (alist-get 'multiple q))
         (options-raw (alist-get 'options q))
         (options (if (vectorp options-raw) (append options-raw nil) options-raw))
         (custom (alist-get 'custom q))
         (prompt (if header
                     (format "[%s] %s: " header text)
                   (format "%s: " text)))
         ;; Build choice list: label + optional description
         (choices (mapcar (lambda (opt)
                            (let ((label (alist-get 'label opt))
                                  (desc (alist-get 'description opt)))
                              (if desc (format "%s (%s)" label desc) label)))
                          options))
         ;; Add "Type your own answer" if custom mode
         (choices (if (or (null custom) custom)
                      (append choices (list "Type your own answer..."))
                    choices)))
    (if multiple
        ;; Multi-select: use minibuffer completing-read with multiple
        (let ((selected '())
               (done nil))
          (while (not done)
            (let* ((remaining (cl-remove-if (lambda (c) (member c selected)) choices))
                   (remaining (append remaining (list "Done (finish selection)")))
                   (choice (completing-read (format "%s [%d selected, choose or Done]: "
                                                    prompt (length selected))
                                            remaining nil nil)))
              (cond
               ((string= choice "Done (finish selection)") (setq done t))
               ((string= choice "Type your own answer...")
                (let ((custom-answer (read-string "Your answer: ")))
                  (push custom-answer selected)))
               (t
                ;; Extract just the label (before " (")
                (let ((label (car (split-string choice " (" t))))
                  (push label selected))))))
          (nreverse selected))
      ;; Single select
      (let* ((choice (completing-read prompt choices nil t))
             (label (cond
                     ((string= choice "Type your own answer...")
                      (read-string "Your answer: "))
                     (t (car (split-string choice " (" t))))))
        (list label)))))

(defun mcp-server-emacs-tools--question-handler (args)
  "Handle question tool invocation with ARGS."
  (condition-case err
      (let* ((questions-raw (alist-get 'questions args))
             (questions (if (vectorp questions-raw) (append questions-raw nil) questions-raw)))
        (unless questions (error "questions is required"))
        (let* ((results (mapcar #'mcp-server-emacs-tools--question-ask-one questions))
               (formatted (cl-mapcar (lambda (q answers)
                                       (format "\"%s\"=\"%s\""
                                               (alist-get 'question q)
                                               (if answers (mapconcat #'identity answers ", ") "Unanswered")))
                                     questions results)))
          (format "User has answered your questions: %s. You can now continue with the user's answers in mind."
                  (mapconcat #'identity formatted ", "))))
    (error (format "Error: %s" (error-message-string err)))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "question"
  :title "Ask Question"
  :description "Use this tool when you need to ask the user questions during execution. This allows you to:
1. Gather user preferences or requirements
2. Clarify ambiguous instructions
3. Get decisions on implementation choices as you work
4. Offer choices to the user about what direction to take.

Usage notes:
- When custom is enabled (default), a \"Type your own answer\" option is added automatically
- Answers are returned as arrays of labels; set multiple: true to allow selecting more than one
- If you recommend a specific option, make that the first option in the list and add \"(Recommended)\" at the end"
  :input-schema '((type . "object")
                  (properties . ((questions . ((type . "array")
                                               (items . ((type . "object")
                                                         (properties . ((question . ((type . "string")
                                                                                     (description . "The question text")))
                                                                        (header . ((type . "string")
                                                                                   (description . "Optional header/title for the question")))
                                                                        (multiple . ((type . "boolean")
                                                                                     (description . "Allow multiple selections")))
                                                                        (custom . ((type . "boolean")
                                                                                   (description . "Allow custom text input (default true)")))
                                                                        (options . ((type . "array")
                                                                                    (items . ((type . "object")
                                                                                              (properties . ((label . ((type . "string")))
                                                                                                             (description . ((type . "string")))))
                                                                                              (required . ["label"])))
                                                                                    (description . "The available options")))))
                                                         (required . ["question" "options"])))
                                               (description . "Questions to ask")))))
                  (required . ["questions"]))
  :function #'mcp-server-emacs-tools--question-handler
  :annotations '((readOnlyHint . :false)
                 (destructiveHint . :false)
                 (idempotentHint . :false)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-question)

;;; mcp-server-emacs-tools-question.el ends here
