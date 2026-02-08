;;; -*- lexical-binding: t; coding: utf-8 -*-

(defconst adh-mono-spaced-font
  (if (eq system-type 'windows-nt)
      "JetBrainsMono NFM Medium"
    "JetBrainsMono Nerd Font Mono"))
(defconst adh-mono-spaced-font-size 80)
(defvar adh--font-hook nil)

(defun adh--apply-font (family height &optional frame)
  (with-selected-frame (or frame (selected-frame))
    (if (and (display-graphic-p)
             (find-font (font-spec :family family)))
        (progn
          (set-face-attribute 'default nil :family family :height height)
          t)
      nil)))

(defun adh--create-parent-dir-on-the-fly ()
  (let ((dir (file-name-directory buffer-file-name)))
    (when (and dir (not (file-exists-p dir)))
      (make-directory dir t)))
  nil)

(defun adh-set-font (family height)
  (when adh--font-hook (remove-hook 'after-make-frame-functions adh--font-hook))
  (setq adh--font-hook
        (lambda (frame)
          (unless (adh--apply-font family height frame)
            (when (display-graphic-p frame)
              (message "[adh] Warning: Queued font '%s' not found." family)))))
  (add-hook 'after-make-frame-functions adh--font-hook)
  (if (adh--apply-font family height (selected-frame))
      (message "[adh] Set font '%s'" family)
    (if (display-graphic-p)
        (message "[adh] Font not found: '%s'" family)
      (message "[adh] Queued font '%s'" family))))

(defun adh-add-to-path (path)
  (let ((expanded-path (expand-file-name path)))
    (add-to-list 'exec-path expanded-path)
    (let ((current-path-list (split-string (getenv "PATH") path-separator)))
      (unless (member expanded-path current-path-list)
        (setenv "PATH" (concat expanded-path path-separator (getenv "PATH")))))))

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
  (split-height-threshold 160)

  (whitespace-line-column 10000)
  (whitespace-style
   '(face tabs spaces trailing lines-tail space-before-tab indentation
          newline empty space-after-tab space-mark tab-mark))

  (completion-show-help nil)
  (completions-max-height 30)

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

  (add-to-list 'default-frame-alist '(undecorated . t))
  (setq frame-title-format
        '(:eval (if buffer-file-name
                    (concat (file-name-nondirectory buffer-file-name)
                            (when (buffer-modified-p) "*")
                            " — GNU Emacs")
                  (concat "%b — GNU Emacs"))))

  (adh-set-font adh-mono-spaced-font adh-mono-spaced-font-size)

  (adh-add-to-path "~/bin")

  (setq display-buffer-alist
        `((adh--sidewin-target-p
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (window-height . 30)
           (window-parameters . ((no-other-window . t)))
           (body-function . select-window))))

  (setq-default truncate-lines t)

  (delete-selection-mode 1)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (global-whitespace-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (winner-mode 1)
  :hook
  (emacs-startup . (lambda () (tab-bar-rename-tab "dev") (message "[adh] Activated %d packages in %s" (length package-activated-list) (emacs-init-time))))
  (find-file-not-found-functions . adh--create-parent-dir-on-the-fly)
  (text-mode . visual-line-mode)
  (before-save . delete-trailing-whitespace))

(provide 'adh-emacs)
