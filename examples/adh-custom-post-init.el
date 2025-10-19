;;; -*- lexical-binding: t; coding: utf-8 -*-

(adh-persp-enable-dashboard)
(adh-persp-enable-buffer-narrow)

(adh-set-font adh-mono-spaced-font 80)

(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path "C:/Program Files/Git/usr/bin")
  (with-eval-after-load 'magit
    (setq magit-git-executable "C:/Program Files/Git/bin/git.exe")))
