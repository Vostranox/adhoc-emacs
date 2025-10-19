;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh--rename-isearch-occur-buffer (&rest _)
  "Rename the *Occur* buffer after `isearch-occur' to include the search string."
  (when (get-buffer "*Occur*")
    (with-current-buffer "*Occur*"
      (rename-buffer (format "*s-occur: %s*" isearch-string) t))))

(defun adh--isearch-with-region (forward)
  "Start isearch in FORWARD direction, pre-filled with the active region if any."
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

(defun adh-enable-vc ()
  "Turn the built-in VC backends on."
  (interactive)
  (setq adh--vc-enabled t)
  (setq vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git Hg)))

(defun adh-disable-vc ()
  "Disable all built-in VC backends."
  (interactive)
  (setq adh--vc-enabled nil)
  (setq vc-handled-backends nil))

(defun adh-toggle-vc-mode ()
  "Toggle the built-in VC backends on or off."
  (interactive)
  (if adh--vc-enabled
      (adh-disable-vc)
    (adh-enable-vc)))

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  :config
  (advice-add #'treesit-forward-sexp :override #'forward-sexp-default-function))

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
  (dired-listing-switches "-alh --group-directories-first --sort=version")
  :hook
  ;; Name Dired buffers "*d: repo/relative-path*" and register the dir with zoxide.
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

(use-package bookmark
  :init
  (setq bookmark-default-file (no-littering-expand-var-file-name "bookmarks")))

(use-package savehist
  :init
  (setq savehist-file (no-littering-expand-var-file-name "history"))
  :config
  (savehist-mode 1))

(use-package recentf
  :init
  (setq recentf-save-file (no-littering-expand-var-file-name "recentf"))
  :custom
  (recentf-exclude '("^/tmp"))
  (recentf-max-menu-items 10)
  (recentf-max-saved-items 5000)
  :config
  (defun recentf-open ()
    "Open a recently visited file (overrides the built-in menu-style command).
Already-open files just switch to their buffer.  Marginalia annotates the
candidates as files because `recentf-open' is in `marginalia-command-categories'."
    (interactive)
    (require 'consult)
    (find-file (completing-read "Open: " (mapcar #'consult--fast-abbreviate-file-name recentf-list) nil t)))
  (recentf-mode 1))

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

(use-package transient
  :init
  (setq transient-history-file (no-littering-expand-var-file-name "transient/history.el")
        transient-levels-file (no-littering-expand-var-file-name "transient/levels.el")
        transient-values-file (no-littering-expand-var-file-name "transient/values.el")))

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
  ;; Drop the rule that paints tree-sitter ERROR nodes red, so incomplete code
  ;; while typing isn't a wall of error-face.
  (setq rust-ts-mode--font-lock-settings
        (cl-remove-if
         (lambda (entry)
           (eq (nth 2 entry) 'error))
         rust-ts-mode--font-lock-settings)))

(provide 'adh-core-packages)
