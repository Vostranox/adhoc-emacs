;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-options)
(require 'adh-functions)

(defvar eglot-server-programs)
(defvar eglot-stay-out-of)

(defvar adh--eglot-pending-servers nil
  "Server registrations to apply when Eglot loads, newest first.")

(defvar adh--eglot-global-enabled nil
  "Non-nil when eglot auto-starts in supported `prog-mode' buffers.")

(defun adh--eglot-managed-buffers ()
  "Return the buffers eglot currently manages."
  (seq-filter (lambda (buf) (buffer-local-value 'eglot--managed-mode buf))
              (and (featurep 'eglot) (buffer-list))))

(defun adh--eglot-release-buffer (buf features)
  "Undo eglot's setup for FEATURES in BUF."
  (with-current-buffer buf
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
          (progn (eldoc-mode -1)
                 (when global-eldoc-mode (turn-on-eldoc-mode)))
        (eldoc-mode (if adh--eldoc-mode-before-eglot 1 -1)))
      (kill-local-variable 'adh--eldoc-mode-before-eglot))))

(defun adh--eglot-sync (&rest _)
  "Derive `eglot-stay-out-of' from the settings; reconnect servers on change."
  (let* ((old (bound-and-true-p eglot-stay-out-of))
         (new (append (seq-difference old '(flymake eldoc yas))
                      '(flymake)
                      (unless (eq adh-completion-style 'full) '(eldoc yas))))
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
  "Start eglot here if this is a `prog-mode' buffer whose server is installed."
  (condition-case nil
      (when-let* (((derived-mode-p 'prog-mode))
                  (contact (nth 3 (eglot--guess-contact)))
                  ((or (not (stringp (car-safe contact))) (integerp (cadr contact))
                       (executable-find (car contact) t))))
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
               (handled (seq-difference '(eldoc yas) eglot-stay-out-of)))
           (let ((inhibit-message t))
             (eglot-shutdown-all))
           (dolist (buf bufs)
             (when (buffer-live-p buf)
               (adh--eglot-release-buffer buf handled)
               (when (memq 'eldoc handled)
                 (adh--eglot-settle-eldoc buf))))))))

(defun adh--eglot-format-safe ()
  "Format the buffer via the LSP server, if one manages it and can format."
  (when (and (bound-and-true-p eglot--managed-mode)
             (eglot-server-capable :documentFormattingProvider))
    (eglot-format-buffer)))

(defun adh--lsp-set-format-on-save (on)
  "Format buffers via LSP on save when ON, otherwise stop."
  (if on
      (add-hook 'before-save-hook #'adh--eglot-format-safe)
    (remove-hook 'before-save-hook #'adh--eglot-format-safe)))

(defun adh-register-lsp-server (mode program &rest args)
  "Tell Eglot to run PROGRAM with ARGS for MODE, a mode symbol or list."
  (let ((servers (if (featurep 'eglot) 'eglot-server-programs 'adh--eglot-pending-servers)))
    (set servers (cons (cons mode (cons program args))
                       (assoc-delete-all mode (symbol-value servers) #'equal)))))

(use-package eglot
  :ensure nil :defer 10
  :init
  (adh-register-lsp-server '(c-ts-mode c++-ts-mode) "clangd" "--header-insertion=never")
  :config
  (dolist (entry (reverse adh--eglot-pending-servers))
    (apply #'adh-register-lsp-server (car entry) (cdr entry)))
  (setq adh--eglot-pending-servers nil)
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)
  (add-to-list 'eglot-ignored-server-capabilities :semanticTokensProvider)
  (add-to-list 'eglot-ignored-server-capabilities :documentOnTypeFormattingProvider))

(adh--lsp-set-format-on-save adh-lsp-format-on-save)
(adh--eglot-sync)
(adh--lsp-set-autostart adh-use-lsp)

(provide 'adh-eglot)

;;; adh-eglot.el ends here
