;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'server)

(when (eq system-type 'windows-nt)
  (setq server-use-tcp t)
  (setq server-name "main-server")
  (setq server-auth-dir (concat user-emacs-directory "server/")))

(unless (daemonp)
  (when (server-running-p)
    (server-force-delete))
  (server-start)
  (message "[adh] Emacs server started."))

(provide 'adh-server)
