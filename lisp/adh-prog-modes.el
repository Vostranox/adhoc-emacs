;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-vars)

(defvar treesit-language-source-alist)

(declare-function treesit-auto--build-treesit-source-alist "treesit-auto")

(defun adh-treesit-ensure-grammars ()
  "Install the missing grammars in `adh-treesit-ensured-langs', skipping failures."
  (interactive)
  (require 'treesit-auto)
  (let ((treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))
    (dolist (lang adh-treesit-ensured-langs)
      (unless (treesit-language-available-p lang)
        (with-demoted-errors "[adh] %S" (treesit-install-language-grammar lang))))))

(use-package treesit-auto :ensure t :defer t)

(use-package treesit
  :ensure nil
  :custom
  (treesit-auto-install-grammar 'always)
  :config
  (setq treesit--install-language-grammar-out-dir-history
        (list (no-littering-expand-var-file-name "treesit/")))
  (add-to-list 'treesit-language-source-alist
               '(zig "https://github.com/tree-sitter-grammars/tree-sitter-zig"))
  (add-to-list 'treesit-major-mode-remap-alist '(zig-mode . zig-ts-mode))
  (add-to-list 'treesit-major-mode-remap-alist '(glsl-mode . glsl-ts-mode))
  (customize-set-variable 'treesit-enabled-modes
                          (seq-difference (mapcar #'cdr treesit-major-mode-remap-alist)
                                          adh-treesit-excluded-modes)))

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

(use-package slang-ts-mode
  :vc (:url "https://github.com/Vostranox/slang-ts-mode" :rev :newest)
  :defer t)

(use-package hlsl-ts-mode
  :vc (:url "https://github.com/Vostranox/hlsl-ts-mode" :rev :newest)
  :defer t)

(use-package clang-format :ensure t :defer t)
(use-package cmake-mode :ensure t :defer t)
(use-package glsl-mode :ensure t :defer t)
(use-package go-mode :ensure t :defer t)
(use-package haskell-mode :ensure t :defer t)
(use-package json-mode :ensure t :defer t)
(use-package powershell :ensure t :defer t)
(use-package rust-mode :ensure t :defer t)
(use-package swift-mode :ensure t :defer t)
(use-package typescript-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package zig-mode :ensure t :defer t)

(provide 'adh-prog-modes)
