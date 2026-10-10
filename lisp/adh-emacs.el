;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-options)
(require 'adh-functions)

(defvar ns-use-proxy-icon)

(defvar vertico-count)

(defun adh--apply-font (family height &optional frame)
  "Set the default face to FAMILY at HEIGHT if FRAME can display it.
Return nil on a terminal or if the font is missing."
  (when (and (display-graphic-p frame)
             (find-font (font-spec :family family) frame))
    (set-face-attribute 'default nil :family family :height height)
    (set-face-attribute 'fixed-pitch nil :family family)
    t))

(defun adh--apply-frame-parameter (parameter value)
  "Set frame PARAMETER to VALUE for all current and future frames."
  (modify-all-frames-parameters (list (cons parameter value))))

(defun adh--apply-window-decoration (decorated)
  "Show or hide the window-manager frame decorations per DECORATED."
  (adh--apply-frame-parameter 'undecorated (not decorated)))

(defun adh--apply-frame-opacity (opacity)
  "Set frame OPACITY (0-100); background only, except on Windows and macOS."
  (adh--apply-frame-parameter
   (if (memq system-type '(windows-nt darwin)) 'alpha 'alpha-background)
   opacity)
  (force-mode-line-update t))

(defun adh--apply-list-max-height (lines)
  "Show at most LINES lines in vertico, the *Completions* list and popups."
  (setq vertico-count lines
        completions-max-height lines))

(defun adh--apply-queued-font (frame)
  "Apply `adh-mono-spaced-font' to new FRAME, unhooking once it succeeds."
  (cond ((adh--apply-font adh-mono-spaced-font adh-mono-spaced-font-size frame)
         (remove-hook 'after-make-frame-functions #'adh--apply-queued-font))
        ((display-graphic-p frame)
         (message "[adh] Warning: Queued font '%s' not found." adh-mono-spaced-font))))

(defun adh--apply-font-settings (&rest _)
  "Apply `adh-mono-spaced-font' and its size, also to future frames."
  (if (adh--apply-font adh-mono-spaced-font adh-mono-spaced-font-size)
      (progn
        (remove-hook 'after-make-frame-functions #'adh--apply-queued-font)
        (message "[adh] Set font '%s'" adh-mono-spaced-font))
    (add-hook 'after-make-frame-functions #'adh--apply-queued-font)
    (if (display-graphic-p)
        (message "[adh] Font not found: '%s'" adh-mono-spaced-font)
      (message "[adh] Queued font '%s'" adh-mono-spaced-font))))

(defun adh--apply-electric-pair (on)
  "Enable automatic matching brackets and quotes when ON is non-nil."
  (when (or on (bound-and-true-p electric-pair-mode))
    (electric-pair-mode (if on 1 -1))))

(defun adh--create-parent-dir-on-the-fly ()
  "Create a missing parent directory for the visited file."
  (let ((dir (file-name-directory buffer-file-name)))
    (when (and dir (not (file-exists-p dir)))
      (make-directory dir t)))
  nil)

(defun adh--completions-preselect-first ()
  "Treat the first *Completions* candidate as selected."
  (with-current-buffer standard-output
    (when (get-text-property (point-min) 'mouse-face)
      (let ((inhibit-read-only t))
        (put-text-property (point-min) (1+ (point-min)) 'first-completion t)))))

(defun adh-forward-sexp (&optional arg)
  "Move forward ARG sexps using the syntax table for layout navigation."
  (interactive "^p")
  (let ((forward-sexp-function nil))
    (forward-sexp arg)))

(defun adh-backward-sexp (&optional arg)
  "Move backward ARG sexps using the syntax table for layout navigation."
  (interactive "^p")
  (adh-forward-sexp (- (or arg 1))))

(defun adh--cleanup-whitespace ()
  "Trim trailing whitespace when enabled, preserving Markdown hard breaks."
  (when (and adh-trim-trailing-whitespace
             (not (derived-mode-p 'markdown-mode 'markdown-ts-mode)))
    (delete-trailing-whitespace)))

(define-advice read-buffer-to-switch (:filter-args (_args) adh-short-prompt)
  (list "Switch to: "))

(define-advice list-buffers--refresh (:after (&rest _) adh-minimal)
  (setq tabulated-list-format (seq-remove-at-position tabulated-list-format 4))
  (dolist (entry tabulated-list-entries)
    (setf (cadr entry) (seq-remove-at-position (cadr entry) 4)))
  (when (equal (car tabulated-list-sort-key) "Size")
    (setq tabulated-list-sort-key nil)))

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
  (adh--apply-electric-pair adh-use-electric-pair)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (global-whitespace-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (window-divider-mode 1)
  (tab-bar-history-mode 1)
  :hook
  (emacs-startup . (lambda () (tab-bar-rename-tab "dev") (message "[adh] Activated %d packages in %s" (length package-activated-list) (emacs-init-time))))
  (find-file-not-found-functions . adh--create-parent-dir-on-the-fly)
  (text-mode . visual-line-mode)
  (minibuffer-setup . (lambda () (setq-local yank-excluded-properties t)))
  (completion-list-mode . (lambda () (display-line-numbers-mode -1)))
  (compilation-mode . (lambda () (setq-local scroll-conservatively 101)))
  (compilation-start . (lambda (_) (window--adjust-process-windows)))
  (completion-setup . adh--completions-preselect-first)
  (before-save . adh--cleanup-whitespace))

(provide 'adh-emacs)

;;; adh-emacs.el ends here
