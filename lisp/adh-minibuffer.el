;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-options)

(defvar consult--tofu-regexp)
(defvar vertico-multiform-categories)
(defvar vertico-mouse-map)
(declare-function vertico-next "vertico")

(defun adh-minibuffer-next-history-or-clear (n)
  "Move N steps forward through history, clearing the input past the end."
  (interactive "p")
  (condition-case nil
      (next-history-element n)
    (error (delete-minibuffer-contents))))

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

(defun adh--vertico-wheel-step (event)
  "Return how many candidates the wheel EVENT moves by, or nil."
  (pcase (event-basic-type event)
    ('wheel-down 1)
    ('wheel-up -1)))

(defun adh-vertico-mouse-scroll (event)
  "Move the Vertico selection for the wheel EVENT and those queued behind it."
  (interactive "e")
  (let ((steps (adh--vertico-wheel-step event))
        next)
    (while (and (input-pending-p)
                (setq next (read-event nil nil 0))
                (if-let* ((step (adh--vertico-wheel-step next)))
                    (setq steps (+ steps step))
                  (push next unread-command-events)
                  nil)))
    (vertico-next steps)))

(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (vertico-mouse-mode 1)
  :custom
  (vertico-resize 'grow-only)
  (vertico-flat-max-lines 3)
  (vertico-buffer-display-action
   '(display-buffer-same-window (inhibit-same-window . nil) (body-function . (lambda (win) (delete-other-windows win)))))
  :config
  (adh--apply-vertico-style adh-vertico-style)
  (with-eval-after-load 'vertico-mouse
    (keymap-set vertico-mouse-map "<wheel-down>" #'adh-vertico-mouse-scroll)
    (keymap-set vertico-mouse-map "<wheel-up>" #'adh-vertico-mouse-scroll))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  (minibuffer-setup . vertico-repeat-save))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-ignore-case t)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
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

(provide 'adh-minibuffer)

;;; adh-minibuffer.el ends here
