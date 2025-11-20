;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh--isearch-with-region (forward)
  (if (use-region-p)
      (let ((search-string (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (isearch-mode forward)
        (isearch-yank-string search-string))
    (if forward
        (call-interactively #'isearch-forward)
      (call-interactively #'isearch-backward))))

(defun adh-isearch-forward-with-region ()
  (interactive)
  (adh--isearch-with-region t))

(defun adh-isearch-backward-with-region ()
  (interactive)
  (adh--isearch-with-region nil))

(defun adh-enable-vc ()
  (interactive)
  (setq adh--vc-enabled t)
  (setq vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git Hg)))

(defun adh-disable-vc ()
  (interactive)
  (setq adh--vc-enabled nil)
  (setq vc-handled-backends nil))

(defun adh-toggle-vc-mode ()
  (interactive)
  (if adh--vc-enabled
      (adh-disable-vc)
    (adh-enable-vc)))

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (isearch-case-fold-search t)
  (search-whitespace-regexp ".*?")
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil))

(use-package dired
  :ensure nil
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
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
                         (final-name (format "*d: %s" core-name)))
                    (unless (string= (buffer-name) final-name)
                      (rename-buffer final-name t)))
                  (when (fboundp 'zoxide-add)
                    (zoxide-add)))))


(use-package wdired
  :ensure nil
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

(use-package org
  :ensure nil
  :custom
  (org-startup-folded 'showeverything)
  (org-src-fontify-natively t)
  (org-indent-indentation-per-level 4)
  :hook
  (org-mode-hook . org-indent-mode))

(use-package eldoc
  :ensure nil :defer t
  :config
  (adh--rename-mode 'eldoc-mode " eldoc"))

(use-package vc
  :ensure nil
  :init
  (defconst adh--vc-mode-line-name " vc")
  :custom
  (auto-revert-check-vc-info nil)
  (vc-handled-backends nil)
  :config
  (defvar adh--vc-enabled nil)
  (add-to-list 'minor-mode-alist '(adh--vc-enabled adh--vc-mode-line-name)))

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

(use-package simple
  :ensure nil
  :config
  (adh--rename-mode 'visual-line-mode " wrap"))

(use-package completion-preview
  :ensure nil
  :config
  (adh--rename-mode 'completion-preview-mode " cp"))

(use-package c-ts-mode
  :ensure nil :defer t
  :hook
  (c-ts-mode . (lambda () (setq-local comment-start "// ") (setq-local comment-end ""))))

(use-package rust-ts-mode
  :ensure nil :defer t
  :config
  (setq rust-ts-mode--font-lock-settings
        (cl-remove-if
         (lambda (entry)
           (eq (nth 2 entry) 'error))
         rust-ts-mode--font-lock-settings)))

(provide 'adh-core-packages)
