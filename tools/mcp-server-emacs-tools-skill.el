;;; mcp-server-emacs-tools-skill.el --- Skill Loading MCP Tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; MCP tool for loading specialized skill instructions.
;; Mirrors the opencode skill.ts tool.
;; Skills are SKILL.md files discovered in the configured skill directories.

;;; Code:

(require 'mcp-server-tools)
(require 'cl-lib)

(defcustom mcp-server-emacs-tools-skill-directories nil
  "List of directories to search for SKILL.md files.
Each directory is searched for files named SKILL.md.
The parent directory name becomes the skill name."
  :type '(repeat directory)
  :group 'mcp-server-emacs-tools)

(defcustom mcp-server-emacs-tools-skill-search-paths
  (list (expand-file-name "~/.cursor/skills-cursor")
        (expand-file-name "~/.config/cursor/skills"))
  "Default paths to search for skills."
  :type '(repeat directory)
  :group 'mcp-server-emacs-tools)

(defun mcp-server-emacs-tools--skill-discover ()
  "Find all SKILL.md files in configured directories.
Returns list of (name location content) plists."
  (let ((skills '())
        (search-dirs (append mcp-server-emacs-tools-skill-directories
                             mcp-server-emacs-tools-skill-search-paths)))
    (dolist (dir search-dirs)
      (when (file-directory-p dir)
        ;; Search for SKILL.md files recursively (one level deep per subdir)
        (dolist (subdir (directory-files dir t nil t))
          (when (and (file-directory-p subdir)
                     (not (member (file-name-nondirectory subdir) '("." ".."))))
            (let ((skill-file (expand-file-name "SKILL.md" subdir)))
              (when (file-exists-p skill-file)
                (let* ((name (file-name-nondirectory subdir))
                       (content (with-temp-buffer
                                  (insert-file-contents skill-file)
                                  (buffer-string)))
                       ;; Extract description from first non-empty line after #
                       (desc (let ((lines (split-string content "\n" t)))
                               (or (car (seq-filter
                                         (lambda (l) (and (> (length l) 0)
                                                         (not (string-prefix-p "#" l))))
                                         lines))
                                   name))))
                  (push (list :name name
                              :location skill-file
                              :description desc
                              :content content)
                        skills))))))))
    (nreverse skills)))

(defun mcp-server-emacs-tools--skill-get (name)
  "Find skill by NAME. Returns skill plist or nil."
  (cl-find-if (lambda (s) (string= (plist-get s :name) name))
              (mcp-server-emacs-tools--skill-discover)))

(defun mcp-server-emacs-tools--skill-handler (args)
  "Handle skill tool invocation with ARGS."
  (condition-case err
      (let* ((name (alist-get 'name args)))
        (unless name (error "name is required"))
        (let ((skill (mcp-server-emacs-tools--skill-get name)))
          (unless skill
            (let ((available (mapcar (lambda (s) (plist-get s :name))
                                     (mcp-server-emacs-tools--skill-discover))))
              (error "Skill \"%s\" not found. Available skills: %s"
                     name (if available (mapconcat #'identity available ", ") "none"))))
          (let* ((dir (file-name-directory (plist-get skill :location)))
                 ;; List bundled files (not SKILL.md itself)
                 (bundled-files
                  (condition-case nil
                      (seq-filter
                       (lambda (f) (not (string= (file-name-nondirectory f) "SKILL.md")))
                       (directory-files dir t nil t))
                    (error nil)))
                 (files-str (if bundled-files
                                (mapconcat (lambda (f) (format "<file>%s</file>" f))
                                           (seq-take bundled-files 10) "\n")
                              "")))
            (format "<skill_content name=\"%s\">\n# Skill: %s\n\n%s\n\nBase directory for this skill: %s\n\n<skill_files>\n%s\n</skill_files>\n</skill_content>"
                    name name
                    (string-trim (plist-get skill :content))
                    (file-name-as-directory dir)
                    files-str))))
    (error (format "Error: %s" (error-message-string err)))))

(defun mcp-server-emacs-tools--skill-build-description ()
  "Build the skill tool description listing available skills."
  (let ((skills (mcp-server-emacs-tools--skill-discover)))
    (if (null skills)
        "Load a specialized skill that provides domain-specific instructions and workflows. No skills are currently available."
      (concat
       "Load a specialized skill that provides domain-specific instructions and workflows.\n\n"
       "When you recognize that a task matches one of the available skills listed below, use this tool to load the full skill instructions.\n\n"
       "<available_skills>\n"
       (mapconcat (lambda (s)
                    (format "  <skill>\n    <name>%s</name>\n    <description>%s</description>\n  </skill>"
                            (plist-get s :name) (plist-get s :description)))
                  skills "\n")
       "\n</available_skills>"))))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "skill"
  :title "Load Skill"
  :description (mcp-server-emacs-tools--skill-build-description)
  :input-schema `((type . "object")
                  (properties . ((name . ((type . "string")
                                          (description . ,(let ((skills (mcp-server-emacs-tools--skill-discover)))
                                                            (if skills
                                                                (format "The name of the skill (e.g., %s)"
                                                                        (mapconcat (lambda (s) (format "'%s'" (plist-get s :name)))
                                                                                   (seq-take skills 3) ", "))
                                                              "The name of the skill from available_skills")))))))
                  (required . ["name"]))
  :function #'mcp-server-emacs-tools--skill-handler
  :annotations '((readOnlyHint . t)
                 (destructiveHint . :false)
                 (idempotentHint . t)
                 (openWorldHint . :false))))

(provide 'mcp-server-emacs-tools-skill)

;;; mcp-server-emacs-tools-skill.el ends here
