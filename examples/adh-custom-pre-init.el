;;; -*- lexical-binding: t; coding: utf-8 -*-

(setq url-proxy-services
      '(("http"  . "proxy.example.com:8080")
        ("https" . "proxy.example.com:8080")))

(setq adh-completion-backend 'corfu)
