;;; -*- lexical-binding: t; coding: utf-8 -*-

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install t)
  :config
  (dolist (lang '(cmake glsl))
    (setq treesit-auto-langs (remove lang treesit-auto-langs)))
  (global-treesit-auto-mode 1))

(use-package clang-format :ensure t :defer t)

(use-package rust-mode :ensure t :init (setq rust-mode-map (make-sparse-keymap)))

(use-package cmake-mode :ensure t :defer t :init (setq cmake-mode-map (make-sparse-keymap)))
(use-package glsl-mode :ensure t :defer t :init (setq glsl-mode-map (make-sparse-keymap)))
(use-package go-mode :ensure t :defer t :init (setq go-mode-map (make-sparse-keymap)))
(use-package haskell-mode :ensure t :defer t :init (setq haskell-mode-map (make-sparse-keymap)))
(use-package json-mode :ensure t :defer t :init (setq json-mode-map (make-sparse-keymap)))
(use-package powershell :ensure t :defer t :init (setq powershell-mode-map (make-sparse-keymap)))
(use-package swift-mode :ensure t :defer t :init (setq swift-mode-map (make-sparse-keymap)))
(use-package yaml-mode :ensure t :defer t :init (setq yaml-mode-map (make-sparse-keymap)))
(use-package zig-mode :ensure t :defer t :init (setq zig-mode-map (make-sparse-keymap)))

(use-package markdown-mode
  :ensure t :defer t
  :init
  (setq markdown-mode-map (make-sparse-keymap))
  :config
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq-local paragraph-start "\f\\|[ \t]*$")
              (setq-local paragraph-separate "[ \t\f]*$")
              (setq-local indent-line-function 'indent-to-left-margin)
              (electric-indent-local-mode -1))))

(with-eval-after-load 'nxml-mode
  (when (boundp 'nxml-mode-map)
    (setq nxml-mode-map (make-sparse-keymap))))

(provide 'adh-prog-modes)
