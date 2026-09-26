;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-functions)

(defvar adh--vc-enabled nil)

(define-advice register-val-jump-to (:around (orig val arg) adh-no-file-query-prompt)
  (if (and (consp val) (eq (car val) 'file-query))
      (cl-letf (((symbol-function 'y-or-n-p) #'always))
        (funcall orig val arg))
    (funcall orig val arg)))

(defun adh--rename-isearch-occur-buffer (&rest _)
  "Name the `isearch-occur' buffer after the search string."
  (when (get-buffer "*Occur*")
    (with-current-buffer "*Occur*"
      (rename-buffer (format "*s-occur: %s*" isearch-string) t))))

(defun adh--isearch-with-region (forward)
  "Start isearch in FORWARD direction, seeded with the region."
  (if (use-region-p)
      (let ((search-string (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (isearch-mode forward)
        (isearch-yank-string search-string))
    (call-interactively (if forward #'isearch-forward #'isearch-backward))))

(defun adh-isearch-forward-with-region ()
  "Search forward, starting from the active region if any."
  (interactive)
  (adh--isearch-with-region t))

(defun adh-isearch-backward-with-region ()
  "Search backward, starting from the active region if any."
  (interactive)
  (adh--isearch-with-region nil))

(defun adh--apply-vc (on)
  "Turn the built-in VC backends and `global-diff-hl-mode' ON or off."
  (setq adh--vc-enabled (and on t))
  (if on
      (progn
        (setq vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git Hg))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (and buffer-file-name
                       (not (file-remote-p buffer-file-name)))
              (ignore-errors (vc-refresh-state)))))
        (when (require 'diff-hl nil t)
          (global-diff-hl-mode 1)))
    (setq vc-handled-backends nil)
    (when (bound-and-true-p global-diff-hl-mode)
      (global-diff-hl-mode -1))))

(defun adh-switch-dired-buffer ()
  "Switch to a Dired buffer."
  (interactive)
  (adh-switch-buffer-of-mode 'dired-mode "Dired: "))

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (isearch-case-fold-search t)
  (search-whitespace-regexp ".*?")
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  :config
  (advice-add 'isearch-occur :after #'adh--rename-isearch-occur-buffer))

(use-package dired
  :ensure nil
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh --group-directories-first --sort=version")
  :hook
  (dired-mode . (lambda ()
                  (let* ((git-root   (locate-dominating-file default-directory ".git"))
                         (repo-name  (and git-root
                                          (file-name-nondirectory
                                           (directory-file-name git-root))))
                         (relative   (if git-root
                                         (directory-file-name
                                          (file-relative-name default-directory git-root))
                                       (abbreviate-file-name
                                        (directory-file-name default-directory))))
                         (core-name  (cond
                                      ((not git-root) relative)
                                      ((member relative '("." "./")) repo-name)
                                      (t (format "%s/%s" repo-name relative))))
                         (final-name (file-name-as-directory core-name)))
                    (unless (or (string= (buffer-name) final-name)
                                (bound-and-true-p dirvish-fd-buffer))
                      (rename-buffer final-name t)))
                  (when (fboundp 'zoxide-add)
                    (zoxide-add))))
  (dired-mode . (lambda () (display-line-numbers-mode -1))))

(use-package wdired
  :ensure nil
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(use-package org
  :ensure nil
  :custom
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-startup-folded 'showeverything)
  (org-src-fontify-natively t)
  (org-indent-indentation-per-level 4)
  (org-directory (expand-file-name "org-tasks" user-emacs-directory))
  (org-capture-templates `(("t" "Todo" entry (file ,(expand-file-name "tasks.org" org-directory)) "* TODO %?  :%^g:")))
  (org-agenda-files (list org-directory))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-deadline-warning-days 0)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  :hook
  (org-mode . org-indent-mode))

(use-package bookmark)

(use-package savehist
  :config
  (savehist-mode 1))

(use-package recentf
  :custom
  (recentf-exclude '("^/tmp"))
  (recentf-max-menu-items 10)
  (recentf-max-saved-items 5000)
  :config
  (recentf-mode 1))

(use-package vc
  :ensure nil
  :init
  (defconst adh--vc-mode-line-name " vc")
  :custom
  (auto-revert-check-vc-info nil)
  :config
  (add-to-list 'minor-mode-alist '(adh--vc-enabled adh--vc-mode-line-name)))

(use-package transient)

(use-package glasses
  :ensure nil :defer t
  :custom
  (glasses-separate-parentheses-p nil))

(define-globalized-minor-mode adh-global-glasses-mode glasses-mode
  (lambda () (when (derived-mode-p 'prog-mode) (glasses-mode 1)))
  :group 'adhoc)

(defun adh--apply-subwords (on)
  "Turn subword motion and display ON or off in all buffers."
  (global-subword-mode (if on 1 -1))
  (adh-global-glasses-mode (if on 1 -1)))

(use-package ediff
  :ensure nil :defer t
  :custom
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :hook
  (ediff-quit . (lambda () (global-whitespace-mode 1)))
  (ediff-startup . (lambda () (global-whitespace-mode -1) (meow-insert-mode 1)))
  (ediff-keymap-setup . (lambda ()
                          (keymap-set ediff-mode-map "," #'ediff-next-difference)
                          (keymap-set ediff-mode-map "." #'ediff-previous-difference))))

(adh--apply-vc adh-use-vc)
(adh--apply-subwords adh-subwords)

(provide 'adh-core-packages)
