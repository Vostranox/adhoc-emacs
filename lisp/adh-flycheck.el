;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-options)
(require 'adh-functions)

(defvar flycheck-annotate-current-line-style)
(defvar flycheck-error-list-buffer)

(defun adh--apply-flycheck (on)
  "Turn Flycheck, its Eglot bridge and inline diagnostics ON or off."
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
  "Place current-line diagnostics according to STYLE.
See `adh-flycheck-annotate-style' for the available positions."
  (set-default 'flycheck-annotate-current-line-style style)
  (when (featurep 'flycheck)
    (dolist (buf (buffer-list))
      (when (buffer-local-value 'flycheck-annotate-mode buf)
        (with-current-buffer buf (flycheck-annotate-mode 1))))))

(defun adh-set-flycheck-annotate-style (style)
  "Set the current-line diagnostic position to STYLE for this session.
See `adh-flycheck-annotate-style' for the available positions."
  (interactive
   (list (intern (completing-read "Diagnostic position: " '("below" "eol" "sideline")
                                  nil t nil nil (symbol-name adh-flycheck-annotate-style)))))
  (customize-set-variable 'adh-flycheck-annotate-style style)
  (message "[adh] Diagnostic position: %s" style))

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

(defun adh-flycheck-display-diagnostic ()
  "Show the Flycheck list's diagnostic without leaving the current window."
  (interactive)
  (adh--with-saved-window #'flycheck-error-list-goto-error))

(defun adh-set-flycheck-executable (checker program)
  "Use PROGRAM for Flycheck CHECKER without loading Flycheck.
CHECKER is a Flycheck checker symbol, such as `c/c++-clang'.
A nil PROGRAM restores the checker default.
Buffer-local executable settings take precedence."
  (set-default (intern (format "flycheck-%s-executable" checker)) program))

(use-package flycheck
  :ensure t :defer t
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-idle-change-delay 0.75)
  (flycheck-annotate-other-lines-levels '(error warning))
  (flycheck-error-list-display-buffer-action nil)
  :config
  (advice-add 'flycheck-annotate--make-overlay :filter-return #'adh--flycheck-dim-annotation-padding)
  (advice-add 'flycheck--report-no-checker :override #'ignore)
  :hook
  (flycheck-error-list-after-refresh . adh--flycheck-fit-error-list)
  (flycheck-error-list-mode . (lambda () (setq tab-line-format nil))))

(use-package consult-flycheck
  :ensure t :defer t)

(adh--apply-flycheck-annotate-style adh-flycheck-annotate-style)
(adh--apply-flycheck adh-use-flycheck)

(provide 'adh-flycheck)

;;; adh-flycheck.el ends here
