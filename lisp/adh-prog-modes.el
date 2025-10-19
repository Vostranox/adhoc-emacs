;;; -*- lexical-binding: t; coding: utf-8 -*-

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install t)
  :config
  (dolist (lang '(cmake glsl))
    (setq treesit-auto-langs (remove lang treesit-auto-langs)))
  (global-treesit-auto-mode 1))

(use-package markdown-mode
  :ensure t :defer t
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq-local paragraph-start "\f\\|[ \t]*$")
              (setq-local paragraph-separate "[ \t\f]*$")
              (setq-local indent-line-function 'indent-to-left-margin)
              (electric-indent-local-mode -1))))

(use-package shader-mode
  :ensure t :defer t
  :hook
  (shader-mode . (lambda () (modify-syntax-entry ?_ "_"))))

(use-package clang-format :ensure t :defer t)
(use-package cmake-mode :ensure t :defer t)
(use-package glsl-mode :ensure t :defer t)
(use-package go-mode :ensure t :defer t)
(use-package haskell-mode :ensure t :defer t)
(use-package json-mode :ensure t :defer t)
(use-package powershell :ensure t :defer t)
(use-package rust-mode :ensure t)
(use-package swift-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package zig-mode :ensure t :defer t)

;; Drop each major mode's local keymap so the global AdHoc/meow bindings win
;; uniformly, instead of being shadowed by mode-specific keys.
(dolist (hook '(prog-mode-hook nxml-mode-hook markdown-mode-hook))
  (add-hook hook (lambda () (use-local-map nil))))

(provide 'adh-prog-modes)
