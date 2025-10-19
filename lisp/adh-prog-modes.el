;;; -*- lexical-binding: t; coding: utf-8 -*-

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install t)
  :config
  (dolist (lang '(cmake glsl))
    (setq treesit-auto-langs (remove lang treesit-auto-langs)))
  (global-treesit-auto-mode 1))

(use-package rust-mode :ensure t)

(use-package clang-format :ensure t :defer t)
(use-package cmake-mode :ensure t :defer t)
(use-package glsl-mode :ensure t :defer t)
(use-package go-mode :ensure t :defer t)
(use-package haskell-mode :ensure t :defer t)
(use-package json-mode :ensure t :defer t)
(use-package lua-mode :ensure t :defer t)
(use-package markdown-mode :ensure t :defer t)
(use-package powershell :ensure t :defer t)
(use-package swift-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package zig-mode :ensure t :defer t)

(define-derived-mode adh-glsl-ts-mode c++-ts-mode "adh-glsl-ts-mode")
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . adh-glsl-ts-mode))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(adh-glsl-ts-mode . ("glsl_analyzer"))))

(provide 'adh-prog-modes)
