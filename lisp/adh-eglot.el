;;; -*- lexical-binding: t; coding: utf-8 -*-

(defvar adh--eglot-global-enabled nil)
(defvar adh--eglot-format-on-save-enabled nil)
(defvar adh--eglot-stay-out-of '(flymake eldoc yas))

(defun adh--eglot-ensure-if-supported ()
  (condition-case nil
      (when (and (derived-mode-p 'prog-mode)
                 (eglot--guess-contact))
        (eglot-ensure))
    (error nil)))

(defun adh--eglot-global-disable ()
  (add-to-list 'eglot-stay-out-of 'flymake)
  (when (bound-and-true-p flymake-mode) (flymake-mode 0))
  (when (bound-and-true-p eldoc-mode)   (eldoc-mode 0))
  (when (fboundp 'yas-minor-mode)
    (when (bound-and-true-p yas-minor-mode) (yas-minor-mode 0)))
  (setq adh--eglot-format-on-save-enabled nil)
  (remove-hook 'before-save-hook #'adh-eglot-format-buffer)
  (setq adh--eglot-global-enabled nil)
  (let ((inhibit-message t))
    (eglot-shutdown-all))
  (remove-hook 'prog-mode-hook #'adh--eglot-ensure-if-supported))

(defun adh--eglot-global-enable ()
  (setq adh--eglot-global-enabled t)
  (adh--eglot-ensure-if-supported)
  (add-hook 'prog-mode-hook #'adh--eglot-ensure-if-supported))

(defun adh-eglot-format-buffer ()
  (interactive)
  (when (and (eglot-managed-p)
             (eglot--server-capable :documentFormattingProvider))
    (eglot-format-buffer)
    (when (eq system-type 'windows-nt)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward "\r" nil t)
          (replace-match ""))))))

(defun adh-eglot-enable-format-on-save ()
  (interactive)
  (setq adh--eglot-format-on-save-enabled t)
  (add-hook 'before-save-hook #'adh-eglot-format-buffer))

(defun adh-eglot-disable-format-on-save ()
  (interactive)
  (setq adh--eglot-format-on-save-enabled nil)
  (remove-hook 'before-save-hook #'adh-eglot-format-buffer))

(defun adh-flymake-preview ()
  (interactive)
  (let ((win (selected-window)))
    (call-interactively #'flymake-goto-diagnostic)
    (select-window win)))

(defun adh-eglot-enable-flymake ()
  (interactive)
  (setq eglot-stay-out-of (delq 'flymake eglot-stay-out-of))
  (call-interactively #'eglot-reconnect)
  (flymake-mode 1))

(defun adh-eglot-disable-flymake ()
  (interactive)
  (add-to-list 'eglot-stay-out-of 'flymake)
  (call-interactively #'eglot-reconnect)
  (flymake-mode 0))

(defun adh-toggle-eglot-global ()
  (interactive)
  (require 'eglot)
  (if adh--eglot-global-enabled
      (adh--eglot-global-disable)
    (adh--eglot-global-enable)))

(defun adh-toggle-eglot-format-on-save ()
  (interactive)
  (if adh--eglot-format-on-save-enabled
      (adh-eglot-disable-format-on-save)
    (adh-eglot-enable-format-on-save)))

(defun adh-toggle-eglot-flymake ()
  (interactive)
  (if (memq 'flymake eglot-stay-out-of)
      (adh-eglot-enable-flymake)
    (adh-eglot-disable-flymake)))

(use-package eglot
  :ensure nil :defer 10
  :init
  (setq eglot-stay-out-of adh--eglot-stay-out-of)
  :config
  (add-to-list 'eglot-ignored-server-capabilities :documentOnTypeFormattingProvider)
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
