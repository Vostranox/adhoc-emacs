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
      (kill-region (region-beginning) (region-end) 'region)
    (unless (eobp) (delete-char 1)))
  (meow-insert))

(defun adh--meow-minibuffer-update-cursor ()
  "Match the minibuffer cursor shape to the current meow state."
  (setq cursor-type (if (or (meow-normal-mode-p) (meow-motion-mode-p)) 'box 'bar)))

(defun adh--meow-minibuffer-setup ()
  "Enable modal editing in the minibuffer, starting in insert state."
  (meow-insert-mode 1)
  (setq-local cursor-type 'bar)
  (add-hook 'post-command-hook #'adh--meow-minibuffer-update-cursor nil t)
  (redisplay))

(use-package meow
  :ensure t
  :custom
  (meow-mode-state-list '((conf-mode . normal)
                          (fundamental-mode . normal)
                          (help-mode . normal)
                          (prog-mode . normal)
                          (text-mode . normal)))
  :config
  (meow-global-mode 1)
  (add-hook 'minibuffer-setup-hook #'adh--meow-minibuffer-setup)
  (setq meow-update-cursor-functions-alist (assq-delete-all 'minibufferp meow-update-cursor-functions-alist)))

(provide 'adh-meow)

;;; adh-meow.el ends here
