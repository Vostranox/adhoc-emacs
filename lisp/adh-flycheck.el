;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-vars)
(require 'adh-functions)

(defvar flycheck-annotate-current-line-style)
(defvar flycheck-error-list-buffer)

(defun adh-set-flycheck-executable (checker program)
  "Use PROGRAM for Flycheck CHECKER without loading Flycheck.
CHECKER can be, for example, `c/c++-clang' or `c/c++-clang-tidy'.
A nil PROGRAM restores the checker default; buffer-local settings take precedence."
  (set-default (intern (format "flycheck-%s-executable" checker)) program))

(defun adh--apply-flycheck (on)
  "Enable or disable Flycheck, its Eglot bridge and inline diagnostics."
  (when (or on (featurep 'flycheck))
    (require 'flycheck)
    (global-flycheck-eglot-mode (if on 1 -1))
    (global-flycheck-annotate-mode (if (and on adh-flycheck-annotate) 1 -1))
    (global-flycheck-mode (if on 1 -1))
    (force-mode-line-update t)))

(defun adh--apply-flycheck-annotate (on)
  "Show diagnostics inline when ON and Flycheck is on."
  (when (featurep 'flycheck)
    (global-flycheck-annotate-mode (if (and on adh-use-flycheck) 1 -1))))

(defun adh--apply-flycheck-annotate-style (style)
  "Put the current line's inline diagnostic below it or at its end per STYLE."
  (set-default 'flycheck-annotate-current-line-style style)
  (when (featurep 'flycheck)
    (dolist (buf (buffer-list))
      (when (buffer-local-value 'flycheck-annotate-mode buf)
        (with-current-buffer buf (flycheck-annotate-mode 1))))))

(defun adh-flycheck-display-diagnostic ()
  "Show the Flycheck list's diagnostic without leaving the current window."
  (interactive)
  (adh--with-saved-window #'flycheck-error-list-goto-error))

(defun adh--flycheck-dim-annotation-padding (overlay)
  "Give OVERLAY's leading spaces the code's whitespace face."
  (when-let* ((text (overlay-get overlay 'before-string)))
    (save-match-data
      (when (string-match "\\` +" text)
        (add-face-text-property 0 (match-end 0) 'whitespace-space nil text))))
  overlay)

(defun adh--flycheck-fit-error-list ()
  "Fit the error list's popup window to its rows."
  (when-let* ((win (get-buffer-window flycheck-error-list-buffer)))
    (when (window-parameter win 'window-side)
      (adh--popper-window-height win))))

(use-package flycheck
  :ensure t :defer t
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-idle-change-delay 0.75)
  (flycheck-annotate-other-lines-levels '(error warning))
  (flycheck-error-list-display-buffer-action nil)
  :custom-face
  (flycheck-annotate-error ((t (:inherit flycheck-error-list-error :weight normal :height 0.9))))
  (flycheck-annotate-warning ((t (:inherit flycheck-error-list-warning :weight normal :height 0.9))))
  (flycheck-annotate-info ((t (:inherit flycheck-error-list-info :weight normal :height 0.9))))
  :config
  (advice-add 'flycheck-annotate--make-overlay :filter-return #'adh--flycheck-dim-annotation-padding)
  (advice-add 'flycheck--report-no-checker :override #'ignore)
  :hook
  (flycheck-error-list-after-refresh . adh--flycheck-fit-error-list))

(use-package flycheck-clang-tidy
  :ensure t :after flycheck :demand t
  :custom
  (flycheck-clang-tidy-extra-options "--allow-no-checks")
  :config
  (define-advice flycheck-clang-tidy-find-project-root (:before-until (_checker) adh-project-root)
    "Use the shared project finder even when VC is disabled."
    (adh--get-project-dir))
  (flycheck-clang-tidy-setup)
  (dolist (mode '(c-ts-mode c++-ts-mode))
    (flycheck-add-mode 'c/c++-clang-tidy mode)))

(adh--apply-flycheck-annotate-style adh-flycheck-annotate-style)
(adh--apply-flycheck adh-use-flycheck)

(provide 'adh-flycheck)
