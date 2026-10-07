;;; -*- lexical-binding: t; coding: utf-8 -*-

(setq frame-title-format "GNU Emacs")

(setq adh-tmux-session "session:window.pane")

(setopt adh-completion-style 'minimal
        adh-completion-ui 'popup
        adh-completion-keys 'tab-only
        adh-vertico-style 'flat
        adh-use-lsp nil
        adh-use-flycheck nil
        adh-flycheck-annotate t
        adh-flycheck-annotate-style 'below
        adh-lsp-format-on-save nil
        adh-use-vc nil
        adh-subwords nil
        adh-use-electric-pair nil
        adh-use-which-key nil
        adh-use-dirvish t
        adh-frame-opacity 100
        adh-window-decoration t
        adh-list-max-height 20
        adh-mono-spaced-font-size 110)

(setq shell-file-name "/usr/bin/fish")

(when (eq system-type 'windows-nt)
  (setq shell-file-name "powershell.exe"
        shell-command-switch "-Command")
  (adh-add-to-path "C:/Program Files/Git/bin")
  (adh-add-to-path "C:/Program Files/Git/usr/bin")
  (setq insert-directory-program "C:/Program Files/Git/usr/bin/ls.exe")
  (with-eval-after-load 'magit
    (setq magit-git-executable "C:/Program Files/Git/bin/git.exe")))

(when (eq system-type 'darwin)
  (adh-add-to-path "/opt/homebrew/bin/")
  (setq insert-directory-program "gls"))

(define-derived-mode adh-glsl-mode slang-ts-mode "Glsl")
(adh-set-file-extension-mode "glsl" 'adh-glsl-mode)
(adh-register-lsp-server 'adh-glsl-mode "glsl_analyzer")

(adh-register-lsp-server '(c-ts-mode c++-ts-mode)
                         "/opt/llvm/bin/clangd" "--header-insertion=never")
(adh-set-flycheck-executable 'c/c++-clang "/opt/llvm/bin/clang")
