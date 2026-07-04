;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh--create-parent-dir-on-the-fly ()
  "Create the visited file's parent directory if it is missing.
Hooked into `find-file-not-found-functions' so new files in new folders just work."
  (let ((dir (file-name-directory buffer-file-name)))
    (when (and dir (not (file-exists-p dir)))
      (make-directory dir t)))
  nil)

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
        make-backup-files nil
        backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/")))
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

  (setq-default abbrev-file-name (no-littering-expand-etc-file-name "abbrev_defs"))

  :custom
  (auto-revert-verbose nil)
  (auto-revert-remote-files nil)
  (revert-without-query '(".*"))

  (c-basic-offset 4)
  (c-default-style "bsd")
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'bsd)
  (go-ts-mode-indent-offset 4)

  (compile-command "")
  (compilation-scroll-output t)

  (completion-ignored-extensions (delete ".git/" completion-ignored-extensions))

  (column-number-mode t)
  (display-line-numbers-type 'relative)
  (icon-title-format frame-title-format)
  (visible-bell nil)
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

  (whitespace-line-column 10000)
  (whitespace-style
   '(face tabs spaces trailing lines-tail space-before-tab indentation
          newline empty space-after-tab space-mark tab-mark))

  (completion-show-help nil)
  (completions-max-height 30)
  (completions-format 'one-column)

  (imenu-flatten nil)
  (imenu-max-items 100)
  (imenu-max-item-length 1000)

  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)

  (history-length 5000)

  (tab-bar-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)

  (initial-buffer-choice (lambda () (get-buffer "*scratch*")))
  (initial-scratch-message ";;\n\n")

  (confirm-kill-emacs 'y-or-n-p)

  (adh-tmux-cd-session "dev:eshell")

  (Buffer-menu-name-width 28)
  (Buffer-menu-mode-width 14)
  :config
  (setq-default case-fold-search t
                indent-tabs-mode nil
                tab-width 4)
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (unless (display-graphic-p)
    (set-terminal-coding-system 'utf-8-unix)
    (set-keyboard-coding-system 'utf-8-unix))
  (when (eq system-type 'windows-nt)
    (set-clipboard-coding-system 'utf-16-le)
    (set-selection-coding-system 'utf-16-le)
    (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (when (eq system-type 'darwin)
    (setq ns-use-proxy-icon nil)
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))
  (adh-set-window-decoration adh-window-decoration)
  (adh-set-frame-opacity adh-frame-opacity)
  (setq frame-title-format nil)

  (adh-set-font adh-mono-spaced-font adh-mono-spaced-font-size)

  (adh-add-to-path "~/bin")

  (setq display-buffer-alist
        `((adh--sidewin-target-p
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (window-height . 30)
           (window-parameters . ((no-other-window . t)))
           (body-function . select-window))))

  (setq-default truncate-lines nil)

  (advice-add 'read-buffer-to-switch :filter-args #'(lambda (args) (list "Switch to: ")))

  (define-advice list-buffers--refresh (:after (&rest _) adh-minimal)
    (setq tabulated-list-format
          (vconcat (seq-take tabulated-list-format 4)
                   (seq-drop tabulated-list-format 5)))
    (setq tabulated-list-entries
          (mapcar (lambda (entry)
                    (let ((v (cadr entry)))
                      (list (car entry)
                            (vconcat (seq-take v 4) (seq-drop v 5)))))
                  tabulated-list-entries))
    (tabulated-list-init-header)
    (setq header-line-format nil))

  (delete-selection-mode 1)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (global-whitespace-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (winner-mode 1)
  :hook
  (emacs-startup . (lambda () (tab-bar-rename-tab "dev") (message "[adh] Activated %d packages in %s" (length package-activated-list) (emacs-init-time))))
  ;; Always use the default sexp/word motion, ignoring any mode-installed
  ;; `forward-sexp-function'/`forward-word-function' (e.g. tree-sitter's).
  (after-init . (lambda () (advice-add #'forward-sexp :around (lambda (orig &rest args) (let ((forward-sexp-function nil)) (apply orig args))))))
  (after-init . (lambda () (advice-add #'forward-word :around (lambda (orig &rest args) (let ((forward-word-function nil)) (apply orig args))))))
  (find-file-not-found-functions . adh--create-parent-dir-on-the-fly)
  (text-mode . visual-line-mode)
  (before-save . delete-trailing-whitespace))

(provide 'adh-emacs)
