;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'server)

(when (eq system-type 'windows-nt)
  (setq server-use-tcp t)
  (setq server-name "main-server")
  (setq server-auth-dir (expand-file-name "server/" user-emacs-directory)))

(unless (or (daemonp) noninteractive)
  (if (eq (server-running-p) t)
      (message "[adh] Another Emacs is running the server; not starting one.")
    (server-start)
    (message "[adh] Emacs server started.")))

(provide 'adh-server)
