;;; -*- lexical-binding: t; coding: utf-8 -*-

(add-to-list 'custom-theme-load-path (locate-user-emacs-file "themes/gruber-material-dark"))
(load-theme 'gruber-material-dark-intense :no-confirm)

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
(defvar adh--gc-cons-threshold gc-cons-threshold)
(defvar adh--gc-cons-percentage gc-cons-percentage)
(defvar adh--file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold adh--gc-cons-threshold
                  gc-cons-percentage adh--gc-cons-percentage
                  file-name-handler-alist adh--file-name-handler-alist)))
