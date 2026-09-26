;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-vars)
(require 'adh-functions)

(defun adh-mc-keyboard-quit-dwim ()
  "Exit multiple-cursors if active, otherwise `adh-keyboard-quit-dwim'."
  (interactive)
  (if (bound-and-true-p multiple-cursors-mode)
      (mc/keyboard-quit)
    (adh-keyboard-quit-dwim)))

(defun adh-avy-goto-line-indent ()
  "Jump to a line with avy and land on its first non-blank character."
  (interactive)
  (avy-goto-line)
  (back-to-indentation))

(defun adh--apply-use-dirvish (on)
  "Open new Dired buffers in Dirvish when ON, else in plain Dired."
  (when (and on (not (fboundp 'dirvish-override-dired-mode)))
    (user-error "Dirvish is not installed"))
  (cond (on (dirvish-override-dired-mode 1))
        ((bound-and-true-p dirvish-override-dired-mode)
         (dirvish-override-dired-mode -1))))

(defun adh-switch-dired-dwim ()
  "Jump through Dirvish history if in use, else switch to a Dired buffer."
  (interactive)
  (call-interactively (if (and adh-use-dirvish (fboundp 'dirvish-history-jump))
                          #'dirvish-history-jump
                        #'adh-switch-dired-buffer)))

(use-package zoxide
  :ensure t
  :hook
  (find-file . zoxide-add))

(use-package dirvish
  :vc (:url "https://github.com/Vostranox/dirvish")
  :defer t
  :custom
  (dirvish-use-header-line nil)
  (dirvish-hide-cursor nil)
  (dirvish-use-mode-line nil)
  (dirvish-attributes '(collapse))
  (dirvish-hide-details nil)
  (dirvish-preview-dispatchers '(video image gif archive font pdf))
  (dirvish-cache-dir (no-littering-expand-var-file-name "dirvish/"))
  (dirvish-wdired-cursor nil)
  (dirvish-preview-other-window nil)
  (dirvish-fd-search-icon ":")
  (dirvish-fd-switches "--full-path --hidden --no-ignore --exclude .git --path-separator=/")
  (dirvish-fd-program (let ((fd (locate-user-emacs-file
                                 (concat "opt/fd/bin/fd" (when (eq system-type 'windows-nt) ".exe")))))
                        (if (file-executable-p fd) fd (executable-find "fd"))))
  :init
  (when adh-use-dirvish
    (dirvish-override-dired-mode 1))
  :config
  (with-eval-after-load 'embark
    (setf (alist-get 'file embark-exporters-alist) #'dirvish-embark-export)))

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

(use-package yasnippet
  :ensure t :defer t
  :init
  (setq yas-verbosity 0)
  :commands (yas-minor-mode yas-global-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets :ensure t :after yasnippet)

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

(use-package rainbow-mode
  :ensure t :defer t
  :custom
  (rainbow-x-colors nil))

(use-package sudo-edit
  :ensure t :defer t)

(use-package diff-hl
  :ensure t :defer t
  :config
  (define-advice diff-hl-show-hunk-inline-show (:filter-return (overlay) adh-window-local)
    (when (overlayp overlay)
      (overlay-put overlay 'window (selected-window)))
    overlay)
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package popper
  :ensure t :demand t
  :custom
  (popper-reference-buffers '(adh--popup-buffer-p))
  (popper-display-function #'adh--popper-display)
  (popper-window-height #'adh--popper-window-height)
  (popper-mode-line "")
  :config
  (put 'popper-popup-status 'permanent-local t)
  (popper-mode 1))

(use-package string-inflection
  :ensure t :defer t)

(use-package gruber-material-dark
  :vc (:url "https://github.com/Vostranox/gruber-material-dark")
  :demand t
  :config
  (unless (custom-theme-enabled-p 'gruber-material-dark-intense)
    (load-theme 'gruber-material-dark-intense :no-confirm)))

(provide 'adh-ext-packages)
