;;; -*- lexical-binding: t; coding: utf-8 -*-

(setq url-proxy-services
      '(("http"  . "proxy.example.com:8080")
        ("https" . "proxy.example.com:8080")))

(setq adh-use-custom-keybinds nil)

(setq adh-completion-backend 'corfu)

(setq adh-frame-opacity 90)
(setq adh-window-decoration t)

(setq adh-mono-spaced-font-size 90)
(setq adh-mono-spaced-font "Iosevka Nerd Font Mono")

(when (eq system-type 'windows-nt)
  (setq default-directory (concat (getenv "USERPROFILE") "\\")))
