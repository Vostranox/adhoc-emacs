;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-vars)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install t)
  :config
  (dolist (lang adh-treesit-excluded-langs)
    (setq treesit-auto-langs (remove lang treesit-auto-langs)))
  (add-to-list 'treesit-language-source-alist
               '(zig "https://github.com/tree-sitter-grammars/tree-sitter-zig"))
  (global-treesit-auto-mode 1))

(use-package c-ts-mode
  :ensure nil :defer t
  :hook
  (c-ts-mode . (lambda () (setq-local comment-start "// ") (setq-local comment-end ""))))

(defun adh--treesit-no-error-face ()
  "Don't paint tree-sitter ERROR nodes; incomplete code isn't an error."
  (treesit-font-lock-recompute-features nil '(error)))

(use-package rust-ts-mode
  :ensure nil :defer t
  :hook
  (rust-ts-mode . adh--treesit-no-error-face))

(use-package zig-ts-mode
  :ensure t :defer t
  :hook
  (zig-ts-mode . adh--treesit-no-error-face))

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

(use-package glsl-mode
  :ensure t :defer t
  :init
  (with-eval-after-load 'c-ts-mode
    (unless (fboundp 'c-ts-mode--simple-indent-rules)
      (defun c-ts-mode--simple-indent-rules (mode style)
        (let ((c-ts-mode-indent-style style))
          (c-ts-mode--get-indent-style mode))))))

(use-package slang-ts-mode
  :vc (:url "https://github.com/Vostranox/slang-ts-mode")
  :mode ("\\.slang\\'" "\\.slangh\\'"))

(use-package hlsl-ts-mode
  :vc (:url "https://github.com/Vostranox/hlsl-ts-mode")
  :mode ("\\.hlsl\\'" "\\.hlsli\\'"))

(use-package clang-format :ensure t :defer t)
(use-package cmake-mode :ensure t :defer t)
(use-package go-mode :ensure t :defer t)
(use-package haskell-mode :ensure t :defer t)
(use-package json-mode :ensure t :defer t)
(use-package powershell :ensure t :defer t)
(use-package rust-mode :ensure t)
(use-package swift-mode :ensure t :defer t)
(use-package typescript-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package zig-mode :ensure t :defer t)

;; Drop mode keymaps so global and meow bindings always win.
(dolist (hook '(prog-mode-hook nxml-mode-hook markdown-mode-hook markdown-ts-mode-hook))
  (add-hook hook (lambda () (use-local-map nil))))

(provide 'adh-prog-modes)
