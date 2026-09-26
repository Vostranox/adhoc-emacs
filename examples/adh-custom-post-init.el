;;; -*- lexical-binding: t; coding: utf-8 -*-

(setq frame-title-format "GNU Emacs")

(setq adh-tmux-cd-session "session:window.pane")

(setopt adh-completion-style 'minimal
        adh-completion-ui 'popup
        adh-completion-keys 'tab-only
        adh-use-lsp nil
        adh-lsp-diagnostics nil
        adh-lsp-format-on-save nil
        adh-use-vc nil
        adh-subwords nil
        adh-use-dirvish t
        adh-frame-opacity 100
        adh-window-decoration t
        adh-list-max-height 20
        adh-mono-spaced-font-size 110)

(when (eq system-type 'windows-nt)
  (adh-add-to-path "C:/Program Files/Git/bin")
  (adh-add-to-path "C:/Program Files/Git/usr/bin")
  (setq insert-directory-program "C:/Program Files/Git/usr/bin/ls.exe")
  (with-eval-after-load 'magit
    (setq magit-git-executable "C:/Program Files/Git/bin/git.exe")))

(when (eq system-type 'darwin)
  (adh-add-to-path "/opt/homebrew/bin/")
  (setq insert-directory-program "gls"))

(define-derived-mode adh-glsl-mode shader-mode "Glsl")
(adh-set-file-extension-mode "glsl" 'adh-glsl-mode)
(adh-register-lsp-server 'adh-glsl-mode "glsl_analyzer")
