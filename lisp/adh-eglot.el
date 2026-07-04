;;; -*- lexical-binding: t; coding: utf-8 -*-

(defvar adh--eglot-global-enabled nil
  "Non-nil when eglot is set to auto-start in every supported `prog-mode' buffer.")

(defun adh--eglot-ensure-if-supported ()
  "Start eglot in the current buffer if it is a prog-mode with a known server."
  (condition-case nil
      (when (and (derived-mode-p 'prog-mode)
                 (eglot--guess-contact))
        (eglot-ensure))
    (error nil)))

(defun adh--eglot-format-safe ()
  "Format the buffer via the LSP server, but only if one is managing it."
  (when (eglot-managed-p)
    (eglot-format-buffer)))

(defun adh--eglot-global-disable ()
  "Turn off global eglot: shut down all servers and remove the auto-start hooks."
  (add-to-list 'eglot-stay-out-of 'flymake)
  (when (bound-and-true-p flymake-mode) (flymake-mode 0))
  (when (bound-and-true-p eldoc-mode) (eldoc-mode 0))
  (when (bound-and-true-p yas-minor-mode) (yas-minor-mode 0))
  (remove-hook 'before-save-hook #'adh--eglot-format-safe)
  (setq adh--eglot-global-enabled nil)
  (let ((inhibit-message t))
    (eglot-shutdown-all))
  (remove-hook 'prog-mode-hook #'adh--eglot-ensure-if-supported))

(defun adh--eglot-global-enable ()
  "Turn on global eglot: start it now and on every future `prog-mode' buffer."
  (setq adh--eglot-global-enabled t)
  (adh--eglot-ensure-if-supported)
  (add-hook 'prog-mode-hook #'adh--eglot-ensure-if-supported))

(defun adh--eglot-enable-flymake ()
  "Let eglot drive flymake and reconnect the current server so diagnostics appear."
  (setq eglot-stay-out-of (delq 'flymake eglot-stay-out-of))
  (when (eglot-current-server)
    (call-interactively #'eglot-reconnect)
    (flymake-mode 1)))

(defun adh--eglot-disable-flymake ()
  "Keep eglot out of flymake and reconnect the current server to drop diagnostics."
  (add-to-list 'eglot-stay-out-of 'flymake)
  (when (bound-and-true-p flymake-mode)
    (call-interactively #'eglot-reconnect)
    (flymake-mode 0)))

(defun adh-set-eglot-stay-out-of (features)
  "Set `eglot-stay-out-of' to FEATURES and reconnect any running server."
  (setq eglot-stay-out-of features)
  (message "[adh] eglot-stay-out-of set to: %s" features)
  (when (eglot-current-server)
    (call-interactively #'eglot-reconnect)))

(defun adh-register-lsp-server (mode program &rest args)
  "Tell eglot to run PROGRAM (with ARGS) as the LSP server for major MODE."
  (let ((cmd-list (cons program args)))
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs (cons mode cmd-list)))))

(defun adh-flymake-display-diagnostic ()
  "Show the flymake diagnostic at point without leaving the current window."
  (interactive)
  (adh--with-saved-window #'flymake-goto-diagnostic))

(defun adh-toggle-eglot-global ()
  "Toggle auto-starting eglot in all supported buffers."
  (interactive)
  (require 'eglot)
  (if adh--eglot-global-enabled
      (adh--eglot-global-disable)
    (adh--eglot-global-enable)))

(defun adh-toggle-eglot-format-on-save ()
  "Toggle LSP formatting of the buffer on every save."
  (interactive)
  (if (member #'adh--eglot-format-safe (default-value 'before-save-hook))
      (progn
        (remove-hook 'before-save-hook #'adh--eglot-format-safe)
        (message "[adh] format on save disabled"))
    (progn
      (add-hook 'before-save-hook #'adh--eglot-format-safe)
      (message "[adh] format on save enabled"))))

(defun adh-toggle-eglot-flymake ()
  "Toggle whether eglot feeds diagnostics to flymake."
  (interactive)
  (if (memq #'flymake eglot-stay-out-of)
      (adh--eglot-enable-flymake)
    (adh--eglot-disable-flymake)))

(use-package eglot
  :ensure nil :defer 10
  :init
  (setq eglot-stay-out-of '(flymake eldoc yas))
  :config
  (defconst adh--eglot-mode-line-name " lsp")
  (add-to-list 'minor-mode-alist '(adh--eglot-global-enabled adh--eglot-mode-line-name))
  (add-to-list 'eglot-ignored-server-capabilities :documentOnTypeFormattingProvider)
  (adh-register-lsp-server '(c-ts-mode c++-ts-mode) "clangd" "--header-insertion=never")
  :hook
  (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1))))

(use-package flymake
  :ensure nil :defer t
  :custom
  (flymake-mode-line-counter-format
   '("(" flymake-mode-line-error-counter
     flymake-mode-line-warning-counter
     flymake-mode-line-note-counter ")"))
  (flymake-mode-line-format
   '(" " flymake-mode-line-exception flymake-mode-line-counters))
  :config
  (adh--rename-mode 'flymake-mode ""))

(provide 'adh-eglot)
