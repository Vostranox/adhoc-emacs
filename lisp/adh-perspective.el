;;; -*- lexical-binding: t; coding: utf-8 -*-

(defvar adh--persp-buffer-narrow-enabled nil)
(defvar adh--persp-use-dashboard nil)

(defun adh--persp-setup-new ()
  (interactive)
  (when (and (fboundp 'dashboard-open)
             adh--persp-use-dashboard)
    (dashboard-open)))

(defun adh-persp-enable-dashboard ()
  (interactive)
  (require 'dashboard)
  (setq adh--persp-use-dashboard t))

(defun adh-kill-persp-other-buffers ()
  (interactive)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
    (persp-kill-other-buffers)))

(defun adh-persp-toggle-buffer-narrow ()
  (interactive)
  (if adh--persp-buffer-narrow-enabled
      (progn
        (setq consult-buffer-sources (delete 'persp-consult-source consult-buffer-sources))
        (consult-customize consult--source-buffer :name "λ" :hidden nil :narrow nil)
        (setq adh--persp-buffer-narrow-enabled nil)
        (message "[adh] Perspective narrow disabled"))
    (add-to-list 'consult-buffer-sources 'persp-consult-source)
    (consult-customize persp-consult-source :name "λ" :narrow nil
                       consult--source-buffer :name "λ" :hidden t :narrow ?r)
    (setq adh--persp-buffer-narrow-enabled t)
    (message "[adh] Perspective narrow enabled")))

(use-package perspective
  :ensure t :demand t :after consult
  :init
  (setq persp-mode-prefix-key (kbd "C-c M-p"))
  :custom
  (persp-sort 'access)
  (persp-initial-frame-name "dev")
  (persp-modestring-dividers '("(" ")" " "))
  :config
  (persp-mode 1)
  :hook
  (persp-created . adh--persp-setup-new))

(provide 'adh-perspective)
