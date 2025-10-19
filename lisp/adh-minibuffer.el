;;; -*- lexical-binding: t; coding: utf-8 -*-

(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  :custom
  (vertico-count 20)
  (vertico-resize 'grow-only)
  (vertico-flat-max-lines 3)
  (vertico-multiform-categories '((t flat)))
  (vertico-buffer-display-action
   '(display-buffer-same-window (inhibit-same-window . nil) (body-function . (lambda (win) (delete-other-windows win)))))
  :config
  (setq vertico-multiform-commands'())
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :custom
  (completion-ignore-case t)
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles . (partial-completion)))))
  :config
  (with-eval-after-load 'consult
    (defun adh--orderless-dollar-tofu-dispatcher (pattern _index _total)
      (when (and (string-suffix-p "$" pattern)
                 (> (length pattern) 1))
        (let* ((tofu (format "[%c-%c]" consult--tofu-char
                             (+ consult--tofu-char consult--tofu-range -1)))
               (core (substring pattern 0 -1)))
          `(orderless-regexp . ,(concat core tofu "*$")))))
    (add-to-list 'orderless-style-dispatchers #'adh--orderless-dollar-tofu-dispatcher)))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

(provide 'adh-minibuffer)
