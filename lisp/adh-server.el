;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'server)

(when (eq system-type 'windows-nt)
  (setq server-use-tcp t)
  (setq server-name "main-server")
  (setq server-auth-dir (expand-file-name "server/" user-emacs-directory)))

(unless (daemonp)
  (when (server-running-p)
    (server-force-delete))
  (server-start)
  (message "[adh] Emacs server started."))

(provide 'adh-server)
