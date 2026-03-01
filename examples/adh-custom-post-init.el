;;; -*- lexical-binding: t; coding: utf-8 -*-

(adh-set-completion-backend 'company)
(adh-set-font adh-mono-spaced-font 80)

(set-frame-parameter nil 'alpha '(90 . 90))

(add-hook 'window-setup-hook (lambda() (set-frame-parameter nil 'undecorated nil)))

(when (eq system-type 'windows-nt)
  (adh-add-to-path "C:/Program Files/Git/bin")
  (adh-add-to-path "C:/Program Files/Git/usr/bin")
  (with-eval-after-load 'magit
    (setq magit-git-executable "C:/Program Files/Git/bin/git.exe")))
