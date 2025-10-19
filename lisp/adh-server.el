;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'server)

(if (or (daemonp) (server-running-p))
    (message "[adh] Emacs server already running.")
  (progn
    (server-start)
    (message "[adh] Emacs server started.")))

(provide 'adh-server)
