;;; -*- lexical-binding: t; coding: utf-8 -*-

(autoload 'project-prompt-project-dir "project" nil t)

(defun adh--get-project-dir (&optional dir)
  "Return the project root above DIR (or `default-directory'), or nil.
A project is the nearest ancestor containing a .git or a .project file."
  (locate-dominating-file (or dir default-directory)
   (lambda (d)
     (or (file-exists-p (file-name-concat d ".git"))
         (file-regular-p (file-name-concat d ".project"))))))

(defun adh--project-try (&optional dir)
  "Project.el backend: return the project containing DIR as a transient project."
  (when-let* ((root (adh--get-project-dir dir)))
    (cons 'transient (expand-file-name root))))

(defun adh-project-compile ()
  "Run `compile' from the project root, or `default-directory' if none."
  (interactive)
  (if-let* ((proj-dir (adh--get-project-dir)))
      (let ((default-directory proj-dir))
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
  "Run `async-shell-command' from the project root, or `default-directory' if none."
  (interactive)
  (if-let* ((proj-dir (adh--get-project-dir)))
      (let ((default-directory proj-dir))
        (call-interactively #'async-shell-command))
    (call-interactively #'async-shell-command)))

(defun adh-project-switch-to-dired ()
  "Prompt for a known project and open its root in Dired, remembering it."
  (interactive)
  (let* ((root (file-name-as-directory
                (expand-file-name (project-prompt-project-dir))))
         (proj (cons 'transient root)))
    (ignore-errors (project-remember-project proj))
    (let ((default-directory root))
      (if (fboundp 'project-dired)
          (call-interactively #'project-dired)
        (dired default-directory)))))

(use-package project
  :ensure nil :defer t
  :init
  (setq project-list-file (no-littering-expand-var-file-name "projects"))
  :config
  (setq project-find-functions #'adh--project-try))

(provide 'adh-project)
