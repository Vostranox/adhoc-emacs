;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh-mc-keyboard-quit-dwim ()
  (interactive)
  (if (bound-and-true-p multiple-cursors-mode)
      (mc/keyboard-quit)
    (adh-keyboard-quit-dwim)))

(use-package zoxide
  :ensure t
  :hook
  ((find-file dired-file) . zoxide-add))

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
  :ensure t :defer 5
  :config
  (define-advice windower-toggle-single
      (:around (orig-fn &rest args) adh-use-adh-delete-other)
    (cl-letf (((symbol-function 'delete-other-windows)
               #'adh-delete-other-windows))
      (apply orig-fn args))))

(use-package rainbow-mode
  :ensure t :defer t
  :custom
  (rainbow-x-colors nil))

(use-package sudo-edit
  :ensure t :defer t)

(provide 'adh-ext-packages)
