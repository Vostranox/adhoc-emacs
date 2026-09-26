;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-vars)
(require 'adh-functions)

(defvar eglot-server-programs)
(defvar eglot-stay-out-of)
(defvar eglot--managed-mode)
(defvar yas-minor-mode)

(defvar adh--eglot-global-enabled nil
  "Non-nil when eglot auto-starts in supported `prog-mode' buffers.")

(defun adh--eglot-managed-buffers ()
  "Return the buffers eglot currently manages."
  (seq-filter (lambda (buf) (buffer-local-value 'eglot--managed-mode buf))
              (and (featurep 'eglot) (buffer-list))))

(defun adh--eglot-stay-out-of-wanted (current)
  "Return CURRENT with flymake, eldoc and yas set from the settings."
  (append (seq-difference current '(flymake eldoc yas))
          (unless adh-lsp-diagnostics '(flymake))
          (unless (eq adh-completion-style 'full)
            '(eldoc yas))))

(defun adh--eglot-release-buffer (buf features)
  "Undo eglot's setup for FEATURES in BUF."
  (with-current-buffer buf
    (when (memq 'flymake features)
      (flymake-mode -1))
    (when (memq 'eldoc features)
      (dolist (f eldoc-documentation-functions)
        (when (and (symbolp f) (string-prefix-p "eglot-" (symbol-name f)))
          (remove-hook 'eldoc-documentation-functions f t)))
      (when (and (local-variable-p 'eldoc-documentation-functions)
                 (equal eldoc-documentation-functions '(t)))
        (kill-local-variable 'eldoc-documentation-functions)))
    (when (and (memq 'yas features) (bound-and-true-p yas-minor-mode))
      (yas-exit-all-snippets)
      (yas-minor-mode -1))))

(defvar-local adh--eldoc-mode-before-eglot 'unknown
  "`eldoc-mode' before eglot took over eldoc, or `unknown'.")

(defun adh--eglot-settle-eldoc (buf)
  "Restore `eldoc-mode' in BUF to its state before eglot."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (local-variable-p 'eldoc-documentation-strategy)
                 (equal eldoc-documentation-strategy
                        (default-value 'eldoc-documentation-strategy)))
        (kill-local-variable 'eldoc-documentation-strategy))
      (if (eq adh--eldoc-mode-before-eglot 'unknown)
          ;; Opened under eglot: decide like `global-eldoc-mode'.
          (progn (eldoc-mode -1)
                 (when global-eldoc-mode (turn-on-eldoc-mode)))
        (eldoc-mode (if adh--eldoc-mode-before-eglot 1 -1)))
      (kill-local-variable 'adh--eldoc-mode-before-eglot))))

(defun adh--eglot-sync ()
  "Derive `eglot-stay-out-of' from the settings; reconnect servers on change."
  (let* ((old (bound-and-true-p eglot-stay-out-of))
         (new (adh--eglot-stay-out-of-wanted old))
         (released (seq-difference new old))
         (bufs (unless (seq-set-equal-p old new) (adh--eglot-managed-buffers))))
    (when (memq 'eldoc (seq-difference old new))
      (dolist (buf bufs)
        (with-current-buffer buf
          (setq adh--eldoc-mode-before-eglot (and eldoc-mode t)))))
    (dolist (buf bufs) (adh--eglot-release-buffer buf released))
    (setq eglot-stay-out-of new)
    (mapc #'eglot-reconnect
          (seq-uniq (delq nil (mapcar (lambda (buf) (with-current-buffer buf (eglot-current-server)))
                                      bufs))))
    (when (memq 'eldoc released)
      (mapc #'adh--eglot-settle-eldoc bufs))))

(defun adh--eglot-ensure-if-supported ()
  "Start eglot here if this is a `prog-mode' buffer with a known server."
  (condition-case nil
      (when (and (derived-mode-p 'prog-mode)
                 (eglot--guess-contact))
        (eglot-ensure))
    (error nil)))

(defun adh--lsp-set-autostart (on)
  "Start eglot in supported buffers when ON, else stop all servers."
  (cond ((and on (not adh--eglot-global-enabled))
         (require 'eglot)
         (setq adh--eglot-global-enabled t)
         (add-hook 'prog-mode-hook #'adh--eglot-ensure-if-supported)
         (adh--eglot-ensure-if-supported))
        ((and (not on) adh--eglot-global-enabled)
         (setq adh--eglot-global-enabled nil)
         (remove-hook 'prog-mode-hook #'adh--eglot-ensure-if-supported)
         (let ((bufs (adh--eglot-managed-buffers))
               (handled (seq-difference '(flymake eldoc yas) eglot-stay-out-of)))
           (let ((inhibit-message t))
             (eglot-shutdown-all))
           (dolist (buf bufs)
             (when (buffer-live-p buf)
               (adh--eglot-release-buffer buf handled)
               (when (memq 'eldoc handled)
                 (adh--eglot-settle-eldoc buf))))))))

(defun adh--eglot-format-safe ()
  "Format the buffer via the LSP server, but only if one is managing it."
  (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (eglot-format-buffer)))

(defun adh--lsp-set-format-on-save (on)
  "Format buffers via LSP on save when ON, otherwise stop."
  (if on
      (add-hook 'before-save-hook #'adh--eglot-format-safe)
    (remove-hook 'before-save-hook #'adh--eglot-format-safe)))

(defun adh--lsp-set-diagnostics (_on)
  "Resync eglot after `adh-lsp-diagnostics' changes."
  (adh--eglot-sync))

(defun adh-register-lsp-server (mode program &rest args)
  "Tell eglot to run PROGRAM (with ARGS) as the LSP server for major MODE."
  (let ((cmd-list (cons program args)))
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs (cons mode cmd-list)))))

(defun adh-flymake-display-diagnostic ()
  "Show the flymake diagnostic at point without leaving the current window."
  (interactive)
  (adh--with-saved-window #'flymake-goto-diagnostic))

(use-package eglot
  :ensure nil :defer 10
  :config
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)
  (add-to-list 'eglot-ignored-server-capabilities :semanticTokensProvider)
  (add-to-list 'eglot-ignored-server-capabilities :documentOnTypeFormattingProvider)
  (adh-register-lsp-server '(c-ts-mode c++-ts-mode) "clangd" "--header-insertion=never"))

(use-package flymake
  :ensure nil :defer t
  :custom
  (flymake-mode-line-counter-format
   '("(" flymake-mode-line-error-counter
     flymake-mode-line-warning-counter
     flymake-mode-line-note-counter ")"))
  (flymake-mode-line-format
   '(" " flymake-mode-line-exception flymake-mode-line-counters)))

(adh--lsp-set-format-on-save adh-lsp-format-on-save)
(adh--eglot-sync)
(adh--lsp-set-autostart adh-use-lsp)

(provide 'adh-eglot)
