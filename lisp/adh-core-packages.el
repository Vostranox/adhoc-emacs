;;; -*- lexical-binding: t; coding: utf-8 -*-

(defvar adh--vc-enabled nil)

(defun adh--update-split-threshold (frame)
  (with-selected-frame frame
    (let ((width  (frame-total-cols frame))
          (height (* (frame-total-lines frame) 4)))
      (if (>= width height)
          (setq split-width-threshold  (/ width 2)
                split-height-threshold nil)
        (setq split-width-threshold  nil
              split-height-threshold (/ height 2))))))

(defun adh--with-saved-window (fn)
  (let ((win (selected-window)))
    (call-interactively fn)
    (select-window win)))

(defun adh--isearch-with-region (forward)
  (if (use-region-p)
      (let ((search-string (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (isearch-mode forward)
        (isearch-yank-string search-string))
    (if forward
        (isearch-forward)
      (isearch-backward))))

(defun adh-compile-goto-error-and-pop ()
  (interactive)
  (adh--with-saved-window #'compile-goto-error))

(defun adh-occur-goto-and-pop ()
  (interactive)
  (adh--with-saved-window #'occur-mode-goto-occurrence))

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

(use-package emacs
  :ensure nil
  :custom
  (auto-revert-verbose nil)
  (auto-revert-remote-files nil)
  (auto-revert-check-vc-info nil)
  (vc-handled-backends nil)
  (revert-without-query '(".*"))

  (c-basic-offset 4)
  (c-default-style "bsd")
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'bsd)
  (go-ts-mode-indent-offset 4)

  (treesit-font-lock-level 4)

  (compile-command "")

  (column-number-mode t)
  (display-line-numbers-type 'relative)
  (icon-title-format frame-title-format)
  (visible-bell nil)
  (use-short-answers t)

  (enable-recursive-minibuffers t)
  (large-file-warning-threshold (* 5 1024 1024))
  (ring-bell-function 'ignore)
  (scroll-error-top-bottom t)
  (search-upper-case t)
  (window-min-height 1)
  (window-min-width 1)
  (split-width-threshold 230)
  (split-height-threshold nil)

  (whitespace-line-column 10000)
  (whitespace-style
   '(face tabs spaces trailing lines-tail space-before-tab indentation
          newline empty space-after-tab space-mark tab-mark))

  (imenu-flatten nil)
  (imenu-max-items 100)
  (imenu-max-item-length 1000)

  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)

  (history-length 5000)
  (recentf-exclude '("/tmp"))
  (recentf-max-menu-items 10)
  (recentf-max-saved-items 5000)

  (tab-bar-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)

  (initial-scratch-message nil)
  :config
  (setq-default case-fold-search t
                indent-tabs-mode nil
                tab-width 4)

  (define-advice find-file (:before (filename &optional wildcards) adh--make-dir)
    (unless (file-exists-p filename)
      (let ((dir (file-name-directory filename)))
        (unless (file-exists-p dir)
          (make-directory dir t)))))

  (advice-add #'treesit-forward-sexp :override #'forward-sexp-default-function)

  (defconst adh--vc-mode-line-name " vc")
  (add-to-list 'minor-mode-alist '(adh--vc-enabled adh--vc-mode-line-name))

  (add-hook 'window-size-change-functions #'adh--update-split-threshold)

  (delete-selection-mode 1)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (global-whitespace-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (recentf-mode 1)
  (savehist-mode 1)
  (winner-mode 1)
  :hook
  (text-mode . visual-line-mode)
  (before-save . delete-trailing-whitespace))

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

(use-package rust-ts-mode
  :ensure nil :defer t
  :config
  (setq rust-ts-mode--font-lock-settings
        (cl-remove-if
         (lambda (entry)
           (eq (nth 2 entry) 'error))
         rust-ts-mode--font-lock-settings)))

(provide 'adh-core-packages)
