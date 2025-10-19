;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh--persp-setup-new (&rest _)
  (let ((dir (file-name-as-directory adh--persp-default-directory)))
    (make-directory dir t)
    (setq default-directory dir)
    (with-current-buffer (current-buffer)
      (setq default-directory dir)))
  (when (fboundp 'dashboard-open)
    (dashboard-open)))

(defun adh-kill-persp-other-buffers ()
  (interactive)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
    (persp-kill-other-buffers)))

(use-package perspective
  :ensure t :demand t :after consult
  :init
  (setq persp-mode-prefix-key (kbd "C-c M-p"))
  :custom
  (persp-sort 'access)
  (persp-initial-frame-name "dev")
  (persp-modestring-dividers '("(" ")" " "))
  :config
  (defconst adh--persp-default-directory (expand-file-name "~/"))
  (add-to-list 'consult-buffer-sources persp-consult-source)
  (consult-customize persp-consult-source :name "λ" :narrow nil)
  (persp-mode 1)
  :hook
  (persp-created . adh--persp-setup-new))

(provide 'adh-perspective)
