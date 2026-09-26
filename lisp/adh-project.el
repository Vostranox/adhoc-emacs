;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-vars)

(autoload 'project-prompt-project-dir "project" nil t)

(defvar adh--command-origin-dir)

(defun adh--get-project-dir (&optional dir)
  "Return the nearest ancestor of DIR with one of `adh-project-root-markers'."
  (locate-dominating-file (or dir default-directory)
   (lambda (d)
     (seq-some (lambda (marker)
                 (file-exists-p (file-name-concat d marker)))
               adh-project-root-markers))))

(defun adh--project-try (&optional dir)
  "Project.el backend: return DIR's project as a transient project."
  (when-let* ((root (adh--get-project-dir dir)))
    (cons 'transient (expand-file-name root))))

(defun adh-project-compile ()
  "Run `compile' from the project root, or `default-directory' if none."
  (interactive)
  (if-let* ((proj-dir (adh--get-project-dir)))
      (let ((adh--command-origin-dir default-directory)
            (default-directory proj-dir))
        (call-interactively #'compile))
    (call-interactively #'compile)))

(defun adh-project-compile-region (start end)
  "Run the region START..END as a compile command from the project root."
  (interactive "r")
  (if-let* ((proj-dir (adh--get-project-dir)))
      (let ((default-directory proj-dir))
        (compile (buffer-substring-no-properties start end)))
    (compile (buffer-substring-no-properties start end))))

(defun adh-project-async-shell-command ()
  "Run `async-shell-command' from the project root, or here if none."
  (interactive)
  (if-let* ((proj-dir (adh--get-project-dir)))
      (let ((adh--command-origin-dir default-directory)
            (default-directory proj-dir))
        (call-interactively #'async-shell-command))
    (call-interactively #'async-shell-command)))

(define-advice read-shell-command (:filter-args (args) adh-show-dir)
  "Say where compile and shell commands run: the project root or a path."
  (let ((prompt (car args)))
    (when (member prompt '("Compile command: " "Async shell command: " "Shell command: "))
      (setcar args (format-message
                    "%s in %s: " (string-remove-suffix ": " prompt)
                    (if (equal (adh--get-project-dir) default-directory)
                        "project"
                      (format-message "`%s'" (abbreviate-file-name default-directory))))))
    args))

(use-package project
  :ensure nil :defer t
  :config
  (setq project-find-functions #'adh--project-try))

(provide 'adh-project)
