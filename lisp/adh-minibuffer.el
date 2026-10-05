;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-vars)

(defvar consult--tofu-regexp)
(defvar vertico-multiform-categories)

(defun adh--apply-vertico-style (style)
  "Use STYLE for minibuffer completion without a more specific layout."
  (setq vertico-multiform-categories
        (append (assq-delete-all t (copy-sequence vertico-multiform-categories))
                (list (if (eq style 'vertical) '(t) (list t style))))))

(defun adh-set-vertico-style (style)
  "Set the default Vertico layout to STYLE for this session."
  (interactive
   (list (intern (completing-read "Vertico layout: " '("flat" "vertical" "grid" "reverse" "buffer")
                                  nil t nil nil (symbol-name adh-vertico-style)))))
  (customize-set-variable 'adh-vertico-style style)
  (message "[adh] Vertico layout: %s" style))

(defun adh--orderless-dollar-tofu-dispatcher (pattern _index _total)
  "Make a trailing \"$\" in PATTERN tolerate consult's tofu suffix char."
  (when (string-match-p "\\`[^!&].*\\$\\'" pattern)
    `(orderless-regexp . ,(concat (substring pattern 0 -1) consult--tofu-regexp "*$"))))

(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  :custom
  (vertico-resize 'grow-only)
  (vertico-flat-max-lines 3)
  (vertico-multiform-commands '((consult-flycheck)))
  (vertico-buffer-display-action
   '(display-buffer-same-window (inhibit-same-window . nil) (body-function . (lambda (win) (delete-other-windows win)))))
  :config
  (adh--apply-vertico-style adh-vertico-style)
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-ignore-case t)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles orderless partial-completion))))
  :config
  (with-eval-after-load 'consult
    (add-to-list 'orderless-style-dispatchers #'adh--orderless-dollar-tofu-dispatcher)))

(use-package prescient
  :ensure t
  :custom
  (completion-preview-sort-function #'prescient-completion-sort)
  (completions-sort #'prescient-completion-sort)
  :config
  (prescient-persist-mode))

(use-package vertico-prescient
  :ensure t :after vertico
  :custom
  (vertico-prescient-enable-filtering nil)
  :config
  (vertico-prescient-mode))

(use-package corfu-prescient
  :ensure t :after corfu
  :custom
  (corfu-prescient-enable-filtering nil)
  :config
  (corfu-prescient-mode))

(provide 'adh-minibuffer)
