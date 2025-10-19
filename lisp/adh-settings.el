;;; -*- lexical-binding: t; coding: utf-8 -*-

(defconst adh--var-dir (locate-user-emacs-file "var/"))
(make-directory adh--var-dir t)

(setq custom-file (expand-file-name "custom.el" adh--var-dir))
(load custom-file :no-error-if-file-is-missing)

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

(defvar adh--font-hook nil)
(defvar adh-mono-spaced-font
  (if (eq system-type 'windows-nt)
      "JetBrainsMono NFM Medium"
    "JetBrainsMono Nerd Font Mono"))
(defvar adh-mono-spaced-font-size 80)
(adh-set-font adh-mono-spaced-font adh-mono-spaced-font-size)

(setq display-buffer-alist
      `((adh--sidewin-target-p
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (window-height . 30)
         (window-parameters . ((no-other-window . t)))
         (body-function . select-window))))

(provide 'adh-settings)
