;;; -*- lexical-binding: t; coding: utf-8 -*-

(define-derived-mode magit-staging-mode magit-status-mode "magit-staging"
  :group 'magit-status)

(defun magit-staging-refresh-buffer ()
  (magit-insert-section (status)
    (magit-insert-untracked-files)
    (magit-insert-unstaged-changes)
    (magit-insert-staged-changes)))

(defun adh--magit-show-commit-current-file (orig-fun &rest args)
  (if (and (null (nth 2 args))
           (or (bound-and-true-p magit-blame-mode)
               (bound-and-true-p magit-blame-read-only-mode))
           (buffer-file-name))
      (let* ((rel (magit-file-relative-name (buffer-file-name)))
             (rev (nth 0 args))
             (cmdargs (nth 1 args))
             (module (nth 3 args)))
        (funcall orig-fun rev cmdargs (list rel) module))
    (apply orig-fun args)))

(defun adh-magit-staging ()
  (interactive)
  (require 'magit)
  (magit-setup-buffer #'magit-staging-mode #'magit-staging-refresh-buffer))

(defun adh-magit-staging-quick ()
  (interactive)
  (require 'magit)
  (if-let ((buffer
            (and (not current-prefix-arg)
                 (not (magit-get-mode-buffer 'magit-staging-mode nil 'selected))
                 (magit-get-mode-buffer 'magit-staging-mode))))
      (magit-display-buffer buffer)
    (adh-magit-staging)))

(defun adh-magit-show-commit-original ()
  (interactive)
  (let ((had (advice-member-p #'adh--magit-show-commit-current-file
                              'magit-show-commit)))
    (unwind-protect
        (progn
          (when had
            (advice-remove 'magit-show-commit #'adh--magit-show-commit-current-file))
          (call-interactively #'magit-show-commit))
      (when had
        (advice-add 'magit-show-commit :around #'adh--magit-show-commit-current-file)))))

(defun adh-magit-restore-current ()
  (interactive)
  (let ((path (adh-copy-full-path)))
    (when (y-or-n-p (format "Restore changes in %s? " path))
      (magit-call-git "restore" path)
      (when (get-file-buffer path)
        (with-current-buffer (get-file-buffer path)
          (revert-buffer :ignore-auto :noconfirm))))))

(defun adh-magit-switch-or-status ()
  (interactive)
  (let ((magit-buffers (magit-mode-get-buffers)))
    (if magit-buffers
        (call-interactively #'magit-switch-to-repository-buffer-other-window)
      (magit-status-quick))))

(defun adh-magit-diff-preview-file ()
  (interactive)
  (let ((win (selected-window)))
    (call-interactively #'magit-diff-visit-file-other-window)
    (when (window-live-p win)
      (select-window win))))

(defun adh-magit-blame-copy-short-hash ()
  (interactive)
  (magit-blame-copy-hash)
  (when-let ((last (current-kill 0)))
    (kill-new (substring last 0 7))
    (message "%s" (substring last 0 7))))

(defun adh-toggle-magit-blame ()
  (interactive)
  (if (bound-and-true-p magit-blame-mode)
      (magit-blame-mode 0)
    (call-interactively 'magit-blame-addition)))

(use-package magit
  :ensure t :defer 10
  :commands (magit magit-status magit-status-quick)
  :init
  (setq magit-auto-revert-mode nil)
  :custom
  (magit-commit-show-diff nil)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk 'all)
  (magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18))
  (magit-section-initial-visibility-alist
   '((staged . hide) (unstaged . hide) (untracked . hide) (stashes . hide) (unpushed . hide) (unpulled . hide)))
  :config
  (put 'magit-log-mode 'magit-log-default-arguments '("-n1024"))
  (advice-add #'magit-show-commit :around #'adh--magit-show-commit-current-file)
  :hook
  (magit-mode . (lambda () (let ((bn (buffer-name)))
                             (when (string-match "^magit\\(.*\\): \\(.*\\)" bn)
                               (rename-buffer (format "*m%s: %s" (match-string 1 bn) (match-string 2 bn)) t))))))

(provide 'adh-magit)
