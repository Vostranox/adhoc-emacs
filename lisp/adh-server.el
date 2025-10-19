;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'server)

(unless (daemonp)
  (when (server-running-p)
    (server-force-delete))
  (server-start)
  (message "[adh] Emacs server started."))

(provide 'adh-server)
