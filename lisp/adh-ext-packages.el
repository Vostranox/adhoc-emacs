;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh-mc-keyboard-quit-dwim ()
  "Exit multiple-cursors if active, otherwise `adh-keyboard-quit-dwim'."
  (interactive)
  (if (bound-and-true-p multiple-cursors-mode)
      (mc/keyboard-quit)
    (adh-keyboard-quit-dwim)))

(defun adh-avy-goto-line-indent ()
  "Jump to a line with avy and land on its first non-blank character."
  (interactive)
  (avy-goto-line)
  (back-to-indentation))

(use-package zoxide
  :ensure t
  :hook
  (find-file . zoxide-add))

(use-package multiple-cursors
  :ensure t :demand t
  :custom
  (mc/always-run-for-all t)
  :config
  (put 'adh-mc-keyboard-quit-dwim 'mc/cmds-run-once t))

(use-package avy
  :ensure t
  :custom
  (avy-background t)
  (avy-keys '(?n ?r ?t ?s ?g ?y ?h ?a ?e ?i ?l ?d ?c ?f ?o ?u)))

(use-package yasnippet
  :ensure t :defer t
  :init
  (setq yas-verbosity 0)
  :commands (yas-minor-mode yas-global-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets :ensure t :after yasnippet)

(use-package visual-regexp
  :ensure t :defer t
  :custom
  (vr/default-regexp-modifiers '(:I t :M t :S nil)))

(use-package visual-regexp-steroids
  :ensure t :after visual-regexp)

(use-package vundo
  :ensure t :defer t
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package wgrep
  :ensure t :defer t
  :custom
  (wgrep-auto-save-buffer t))

(use-package windower
  :ensure t :defer 5)

(use-package rainbow-mode
  :ensure t :defer t
  :custom
  (rainbow-x-colors nil))

(use-package sudo-edit
  :ensure t :defer t)

(provide 'adh-ext-packages)
