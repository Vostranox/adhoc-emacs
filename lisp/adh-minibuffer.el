;;; -*- lexical-binding: t; coding: utf-8 -*-

(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  :custom
  (vertico-count 25)
  (vertico-resize 'grow-only)
  (vertico-flat-max-lines 3)
  (vertico-multiform-categories '((t flat)))
  (vertico-buffer-display-action
   '(display-buffer-same-window (inhibit-same-window . nil) (body-function . (lambda (win) (delete-other-windows win)))))
  :config
  (setq vertico-multiform-commands nil)
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
  ;; consult appends an invisible "tofu" char to each candidate to keep them
  ;; unique.  A trailing "$" in a pattern would anchor before that char and never
  ;; match, so this dispatcher rewrites "...$" to also allow the tofu suffix.
  (with-eval-after-load 'consult
    (defun adh--orderless-dollar-tofu-dispatcher (pattern _index _total)
      "Make a trailing \"$\" in PATTERN tolerate consult's tofu suffix char."
      (when (and (string-suffix-p "$" pattern)
                 (> (length pattern) 1))
        (let* ((tofu (format "[%c-%c]" consult--tofu-char
                             (+ consult--tofu-char consult--tofu-range -1)))
               (core (substring pattern 0 -1)))
          `(orderless-regexp . ,(concat core tofu "*$")))))
    (add-to-list 'orderless-style-dispatchers #'adh--orderless-dollar-tofu-dispatcher)))

(use-package prescient
  :ensure t
  :custom
  (completion-preview-sort-function #'prescient-completion-sort)
  :config
  ;; Same tofu problem as above, but for prescient's own regexps: let any "$"
  ;; anchor sit before consult's invisible suffix char instead of the true end.
  (define-advice prescient-filter-regexps
      (:filter-return (regexps) tofu-tolerant)
    (let ((tofu (concat (if (boundp 'consult--tofu-regexp)
                            consult--tofu-regexp
                          (format "[%c-%c]" #x100000 #x10fffd))
                        "*")))
      (mapcar (lambda (rx)
                (replace-regexp-in-string
                 "\\([^\\]\\)\\$\\(\\'\\|\\\\[|)]\\)"
                 (concat "\\1" tofu "$" "\\2")
                 rx t))
              regexps)))
  (prescient-persist-mode))

(use-package vertico-prescient
  :ensure t :after vertico
  :custom
  (vertico-prescient-enable-filtering nil)
  (vertico-prescient-enable-sorting   t)
  :config
  (vertico-prescient-mode))

(use-package corfu-prescient
  :ensure t :after corfu
  :custom
  (corfu-prescient-enable-filtering nil)
  (corfu-prescient-enable-sorting   t)
  :config
  (corfu-prescient-mode))

(use-package company-prescient
  :ensure t :after company
  :config
  (company-prescient-mode))

(provide 'adh-minibuffer)
