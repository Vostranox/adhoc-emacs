;;; -*- lexical-binding: t; coding: utf-8 -*-

(defconst adh-mono-spaced-font
  (if (eq system-type 'windows-nt)
      "JetBrainsMono NFM Medium"
    "JetBrainsMono Nerd Font Mono"))
(defconst adh-mono-spaced-font-size 80)
(defvar adh--font-hook nil)

(defun adh--apply-font (family height &optional frame)
  (with-selected-frame (or frame (selected-frame))
    (when (and (display-graphic-p)
               (find-font (font-spec :family family)))
      (set-face-attribute 'default nil :family family :height height) t)))

(defun adh-set-font (family height)
  (when adh--font-hook (remove-hook 'after-make-frame-functions adh--font-hook))
  (setq adh--font-hook (lambda (frame) (adh--apply-font family height frame)))
  (add-hook 'after-make-frame-functions adh--font-hook)
  (if (adh--apply-font family height (selected-frame))
      (message "[adh] Set font \"%s\"" family)
    (if (display-graphic-p)
        (message "[adh] Font not found: %s" family)
      (message "[adh] Queued font \"%s\"" family))))

(use-package emacs
  :ensure nil
  :init
  (defconst adh--var-dir (locate-user-emacs-file "var/"))
  (make-directory adh--var-dir t)

  (setq custom-file (expand-file-name "custom.el" adh--var-dir))
  (load custom-file t)

  (setq create-lockfiles nil
        disabled-command-function nil
        auto-save-default nil
        auto-save-no-message t
        auto-save-file-name-transforms
        `((".*" ,(expand-file-name "autosaves" adh--var-dir) t))
        auto-save-list-file-prefix
        (expand-file-name "autosaves/.saves-" adh--var-dir)
        make-backup-files nil
        backup-directory-alist
        `(("." . ,(expand-file-name "backup" adh--var-dir)))
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

  (setq bookmark-default-file (expand-file-name "bookmarks" adh--var-dir)
        project-list-file (expand-file-name "projects" adh--var-dir)
        recentf-save-file (expand-file-name "recentf" adh--var-dir)
        savehist-file (expand-file-name "history" adh--var-dir)
        abbrev-file-name (expand-file-name "abbrev_defs" adh--var-dir)
        transient-history-file (expand-file-name "transient/history.el" adh--var-dir)
        transient-levels-file  (expand-file-name "transient/levels.el" adh--var-dir)
        transient-values-file  (expand-file-name "transient/values.el" adh--var-dir))
  (when (boundp 'native-comp-eln-load-path)
    (setq native-comp-eln-load-path
          (list (expand-file-name "eln-cache/" adh--var-dir))))
  :custom
  (auto-revert-verbose nil)
  (auto-revert-remote-files nil)
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
  (recentf-exclude '("/tmp"))
  (recentf-max-menu-items 10)
  (recentf-max-saved-items 5000)

  (tab-bar-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)

  (initial-buffer-choice (lambda () (get-buffer "*scratch*")))
  (initial-scratch-message ";;\n\n")
  :config
  (setq-default case-fold-search t
                indent-tabs-mode nil
                tab-width 4)

  (define-advice find-file (:before (filename &optional wildcards) adh--make-dir)
    (unless (file-exists-p filename)
      (let ((dir (file-name-directory filename)))
        (unless (file-exists-p dir)
          (make-directory dir t)))))

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

  (if (fboundp 'adh-set-font)
      (adh-set-font adh-mono-spaced-font adh-mono-spaced-font-size)
    (message "[adh] adh-set-font not found"))

  (setq display-buffer-alist
        `((adh--sidewin-target-p
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (window-height . 30)
           (window-parameters . ((no-other-window . t)))
           (body-function . select-window))))

  (advice-add #'treesit-forward-sexp :override #'forward-sexp-default-function)

  (delete-selection-mode 1)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (global-whitespace-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (recentf-mode 1)
  (savehist-mode 1)
  (winner-mode 1)
  :hook
  (emacs-startup . (lambda () (message "[adh] Activated %d packages in %s" (length package-activated-list) (emacs-init-time))))
  (text-mode . visual-line-mode)
  (before-save . delete-trailing-whitespace))

(provide 'adh-emacs)
