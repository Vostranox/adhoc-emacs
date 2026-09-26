;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-functions)

(declare-function project-try-vc "project" (dir))

(defun adh--project-try (&optional dir)
  "Project.el backend: DIR's project, Git-backed if its root has a .git dir."
  (when-let* ((root (adh--get-project-dir dir)))
    (or (and (file-directory-p (expand-file-name ".git" root))
             (let ((vc-handled-backends '(Git)))
               (project-try-vc root)))
        (cons 'transient (expand-file-name root)))))

(defun adh--run-with-region (command dir)
  "Run COMMAND from DIR, starting its prompt with the active region, if any."
  (let* ((region (and (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))))
         (adh--command-origin-dir default-directory)
         (default-directory dir)
         (adh--shell-prompt-dir dir))
    (minibuffer-with-setup-hook
        (lambda ()
          (when region
            (delete-minibuffer-contents)
            (insert region)))
      (call-interactively command))))

(defun adh-compile-region ()
  "Compile from `default-directory', starting with the active region."
  (interactive)
  (adh--run-with-region #'compile default-directory))

(defun adh-async-shell-command-region ()
  "Run an async shell command from `default-directory', with the active region."
  (interactive)
  (adh--run-with-region #'async-shell-command default-directory))

(defun adh-project-compile-region ()
  "Compile from the project root, starting with the active region."
  (interactive)
  (adh--run-with-region #'compile (or (adh--get-project-dir) default-directory)))

(defun adh-project-async-shell-command-region ()
  "Run an async shell command from the project root, with the active region."
  (interactive)
  (adh--run-with-region #'async-shell-command (or (adh--get-project-dir) default-directory)))

(defun adh--dir-label (dir)
  "Name DIR like consult prompts do: \"Project NAME\" at a root, else the path."
  (let ((root (adh--get-project-dir dir)))
    (if (and root (file-equal-p root dir))
        (concat "Project " (file-name-nondirectory (directory-file-name root)))
      (directory-file-name (abbreviate-file-name dir)))))

(define-advice read-shell-command (:filter-args (args) adh-dir-label)
  "Label the prompt with `adh--shell-prompt-dir' when an adh command sets it."
  (when-let* ((dir adh--shell-prompt-dir)
              (base (pcase (car args)
                      ("Compile command: " "Compile")
                      ("Async shell command: " "Async shell")
                      ("Shell command: " "Shell"))))
    (setcar args (format "%s (%s): " base (adh--dir-label dir))))
  args)

(use-package project
  :ensure nil :defer t
  :config
  (setq project-find-functions #'adh--project-try)
  (cl-defmethod project-external-roots ((_project (head vc))) nil))

(provide 'adh-project)
