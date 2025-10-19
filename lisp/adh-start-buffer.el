;;; -*- lexical-binding: t; coding: utf-8 -*-

(setq initial-buffer-choice (lambda () (get-buffer "*Messages*")))
(defvar adh--loads-at-start 0)
(add-hook 'before-init-hook
          (lambda () (setq adh--loads-at-start (length load-history))))
(add-hook 'after-init-hook
          (lambda ()
            (let* ((elapsed (float-time (time-subtract after-init-time before-init-time)))
                   (loaded  (- (length load-history) adh--loads-at-start)))
              (message "[adh] Loaded %d libraries in %.2f seconds" loaded elapsed))))
(add-hook 'after-init-hook #'emacs-init-time)

(provide 'adh-start-buffer)
