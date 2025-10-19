;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh-toggle-meow-motion-mode ()
  "Switch between meow normal and motion states."
  (interactive)
  (if (meow-motion-mode-p)
      (meow-normal-mode)
    (meow-motion-mode)))

(defun adh-meow-insert ()
  "Drop the region and enter insert state."
  (interactive)
  (deactivate-mark)
  (meow-insert))

(defun adh-meow-insert-replace ()
  "Enter insert state, first deleting the region or the character at point."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (unless (eobp) (delete-char 1)))
  (meow-insert))

(use-package meow
  :ensure t
  :custom
  (meow-expand-hint-remove-delay 0)
  (meow-keypad-describe-keymap-function nil)
  (meow-mode-state-list '((conf-mode . normal)
                          (fundamental-mode . normal)
                          (help-mode . normal)
                          (prog-mode . normal)
                          (text-mode . normal)))
  :config
  (meow-global-mode 1)
  (adh--rename-mode 'meow-normal-mode "")
  (adh--rename-mode 'meow-insert-mode "")
  (adh--rename-mode 'meow-motion-mode ""))

(provide 'adh-meow)
