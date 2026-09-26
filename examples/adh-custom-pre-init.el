;;; -*- lexical-binding: t; coding: utf-8 -*-

(setq url-proxy-services
      '(("http"  . "proxy.example.com:8080")
        ("https" . "proxy.example.com:8080")))

(setq adh-use-custom-keybinds nil)

(setq adh-completion-style 'minimal)
(setq adh-completion-ui 'popup)
(setq adh-completion-keys 'tab-only)
(setq adh-use-lsp nil)
(setq adh-lsp-diagnostics nil)
(setq adh-lsp-format-on-save nil)
(setq adh-use-vc nil)
(setq adh-subwords nil)

(setq adh-use-dirvish nil)

(setq adh-frame-opacity 90)
(setq adh-window-decoration t)

(setq adh-mono-spaced-font-size 90)
(setq adh-mono-spaced-font "Iosevka Nerd Font Mono")

(setq adh-treesit-excluded-langs '(cmake))

(add-to-list 'adh-popup-buffers "\\*Man ")
(setq adh-list-max-height 15)

(when (eq system-type 'windows-nt)
  (setq default-directory (concat (getenv "USERPROFILE") "\\")))
