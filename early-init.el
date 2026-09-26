;;; -*- lexical-binding: t; coding: utf-8 -*-

(let ((dir (locate-user-emacs-file "elpa/gruber-material-dark/")))
  (when (file-exists-p (expand-file-name "gruber-material-dark.el" dir))
    (add-to-list 'custom-theme-load-path dir)
    (add-to-list 'load-path dir)
    (condition-case err
        (load-theme 'gruber-material-dark-intense :no-confirm)
      (error (message "Failed to load theme: %s" (error-message-string err))))))

(blink-cursor-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tab-bar-mode 0)
(tool-bar-mode 0)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(defun display-startup-echo-area-message ()
  (message ""))

;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-emacs.org
(defvar adh--gc-cons-percentage gc-cons-percentage)
(defvar adh--file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage adh--gc-cons-percentage
                  file-name-handler-alist (delete-dups
                                           (append file-name-handler-alist
                                                   adh--file-name-handler-alist)))))

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name "var/eln-cache/" user-emacs-directory)))

(setq load-prefer-newer t)
(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(require 'adh-startup)

(when (adh--config-stale-p)
  (adh-compile-config))
