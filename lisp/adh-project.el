;;; -*- lexical-binding: t; coding: utf-8 -*-

(autoload 'project-prompt-project-dir "project" nil t)

(defun adh--project-root (&optional dir)
  (let ((dir (or dir default-directory)))
    (locate-dominating-file
     dir
     (lambda (d)
       (let ((git (file-name-concat d ".git"))
             (proj (file-name-concat d ".project")))
         (or (file-exists-p git)
             (file-regular-p proj)))))))

(defun adh--get-project-dir ()
  (adh--project-root))

(defun adh--project-try (&optional dir)
  (when-let ((root (adh--project-root dir)))
    (cons 'transient (expand-file-name root))))

(defun adh-project-compile ()
  (interactive)
  (if-let* ((proj-dir (adh--get-project-dir)))
      (let ((default-directory proj-dir))
        (call-interactively #'compile))
    (call-interactively #'compile)))

(defun adh-project-compile-region (start end)
  (interactive "r")
  (if-let* ((proj-dir (adh--get-project-dir)))
      (let ((default-directory proj-dir))
        (compile (buffer-substring-no-properties start end)))
    (compile (buffer-substring-no-properties start end))))

(defun adh-project-async-shell-command ()
  (interactive)
  (if-let* ((proj-dir (adh--get-project-dir)))
      (let ((default-directory proj-dir))
        (call-interactively #'async-shell-command))
    (call-interactively #'async-shell-command)))

(defun adh-project-switch-to-dired ()
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
  :config
  (setq project-find-functions #'adh--project-try))

(provide 'adh-project)
