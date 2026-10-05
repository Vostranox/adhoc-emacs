;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'server)

(when (eq system-type 'windows-nt)
  (setq server-name "main-server"))

(unless (or (daemonp) noninteractive)
  (if (eq (server-running-p) t)
      (message "[adh] Another Emacs server is already running.")
    (server-start)
    (message "[adh] Emacs server started.")))

(provide 'adh-server)

;;; adh-server.el ends here
