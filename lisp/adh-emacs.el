;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-vars)
(require 'adh-functions)

(defvar ns-use-proxy-icon)

(defun adh--create-parent-dir-on-the-fly ()
  "Create a missing parent directory for the visited file."
  (let ((dir (file-name-directory buffer-file-name)))
    (when (and dir (not (file-exists-p dir)))
      (make-directory dir t)))
  nil)

(define-advice forward-sexp (:around (orig &rest args) adh-syntax-only)
  (let ((forward-sexp-function nil))
    (apply orig args)))

(define-advice read-buffer-to-switch (:filter-args (_args) adh-short-prompt)
  (list "Switch to: "))

(define-advice list-buffers--refresh (:after (&rest _) adh-minimal)
  (setq tabulated-list-format (seq-remove-at-position tabulated-list-format 4)
        tabulated-list-entries
        (mapcar (lambda (e) (list (car e) (seq-remove-at-position (cadr e) 4)))
                tabulated-list-entries)))

(define-advice tabulated-list-init-header (:after () adh-buffer-menu)
  (when (derived-mode-p 'Buffer-menu-mode)
    (setq header-line-format nil)))

(use-package emacs
  :ensure nil
  :init
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (load custom-file t)

  (setq create-lockfiles nil
        disabled-command-function nil
        auto-save-default nil
        auto-save-no-message t
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "autosaves/") t))
        auto-save-list-file-prefix
        (no-littering-expand-var-file-name "autosaves/.saves-")
        make-backup-files nil)

  :custom
  (auto-revert-verbose nil)
  (revert-without-query '(".*"))

  (c-basic-offset 4)
  (c-default-style "bsd")
  (c-ts-indent-offset 4)
  (c-ts-mode-indent-style 'bsd)
  (go-ts-indent-offset 4)

  (compile-command "")
  (compilation-scroll-output t)
  (compilation-environment '("NO_COLOR=1"))
  (next-error-find-buffer-function #'next-error-buffer-unnavigated-current)

  (completion-ignored-extensions (delete ".git/" completion-ignored-extensions))

  (column-number-mode t)
  (display-line-numbers-type 'relative)
  (use-short-answers t)

  (duplicate-line-final-position -1)
  (duplicate-region-final-position -1)

  (minibuffer-default-prompt-format "")
  (enable-recursive-minibuffers t)
  (large-file-warning-threshold (* 5 1024 1024))
  (ring-bell-function 'ignore)
  (scroll-error-top-bottom t)
  (search-upper-case t)
  (window-min-height 1)
  (window-min-width 1)
  (split-width-threshold 230)
  (split-height-threshold 160)

  (kill-do-not-save-duplicates t)
  (save-interprogram-paste-before-kill t)

  (whitespace-global-modes '(not magit-mode dired-mode wdired-mode diff-mode))
  (whitespace-style
   '(face tabs spaces trailing space-before-tab indentation
          empty space-after-tab space-mark tab-mark))

  (completion-show-help nil)
  (completions-header-format "")
  (completions-format 'one-column)
  (completions-detailed t)
  (completion-eager-display nil)
  (completion-eager-update t)

  (imenu-max-items 100)
  (imenu-max-item-length 1000)

  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)

  (history-length 5000)

  (tab-bar-show nil)
  (tab-bar-new-tab-choice #'get-scratch-buffer-create)

  (initial-scratch-message ";;\n\n")

  (confirm-kill-emacs 'y-or-n-p)

  (Buffer-menu-name-width 28)
  (Buffer-menu-mode-width 14)
  (window-divider-default-right-width 1)
  :config
  (setq-default indent-tabs-mode nil
                tab-width 4)
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8-unix)
  (unless (display-graphic-p)
    (set-terminal-coding-system 'utf-8-unix)
    (set-keyboard-coding-system 'utf-8-unix))

  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (when (eq system-type 'darwin)
    (setq ns-use-proxy-icon nil
          frame-resize-pixelwise t)
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

  (adh--apply-window-decoration adh-window-decoration)
  (adh--apply-frame-opacity adh-frame-opacity)
  (adh--apply-list-max-height adh-list-max-height)
  (setq frame-title-format nil)

  (adh--apply-font-settings)

  (adh-add-to-path "~/bin")

  (add-to-list 'display-buffer-alist
               '("\\*Completions\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . completions--fit-window-to-buffer)
                 (window-parameters . ((no-other-window . t)))))

  (setq switch-to-prev-buffer-skip
        (lambda (_window buf _bury-or-kill) (not (adh--buffer-listable-p buf))))

  (delete-selection-mode 1)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (global-whitespace-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (window-divider-mode 1)
  (winner-mode 1)
  :hook
  (emacs-startup . (lambda () (tab-bar-rename-tab "dev") (message "[adh] Activated %d packages in %s" (length package-activated-list) (emacs-init-time))))
  (find-file-not-found-functions . adh--create-parent-dir-on-the-fly)
  (text-mode . visual-line-mode)
  (completion-list-mode . (lambda () (display-line-numbers-mode -1)))
  (compilation-mode . (lambda () (setq-local scroll-conservatively 101)))
  (compilation-start . (lambda (_) (window--adjust-process-windows)))
  (completion-setup . adh--completions-preselect-first)
  (before-save . delete-trailing-whitespace))

(provide 'adh-emacs)
