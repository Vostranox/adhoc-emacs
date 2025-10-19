;;; -*- lexical-binding: t; coding: utf-8 -*-

(use-package dashboard
  :ensure t
  :custom
  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-newline
     dashboard-insert-newline
     dashboard-insert-navigator
     dashboard-insert-newline
     dashboard-insert-init-info
     dashboard-insert-items
     dashboard-insert-newline))
  (dashboard-items
  '((recents   . 20)
    (projects  . 10)
    (bookmarks . 10)))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-projects-switch-function
        (lambda (dir)
          (let ((default-directory (file-name-as-directory (expand-file-name dir))))
            (if (fboundp 'project-dired)
                (call-interactively 'project-dired)
              (dired default-directory))))))

(provide 'adh-dashboard)
