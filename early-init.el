;;; -*- lexical-binding: t; coding: utf-8 -*-

(blink-cursor-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(unless (eq system-type 'darwin)
  (setq frame-inhibit-implied-resize t))

(setq inhibit-startup-message t
      inhibit-default-init t)
(defun display-startup-echo-area-message ()
  (message ""))

;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-emacs.org
(defvar adh--file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      load-path-filter-function #'load-path-filter-cache-directory-files
      file-name-handler-alist (let ((jka (rassq 'jka-compr-handler file-name-handler-alist)))
                                (and jka (list jka)))
      vc-handled-backends nil)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  load-path-filter-function nil
                  file-name-handler-alist (delete-dups
                                           (append file-name-handler-alist
                                                   adh--file-name-handler-alist)))))

(when (featurep 'native-compile)
  (startup-redirect-eln-cache "var/eln-cache/"))

(setq package-quickstart-file (locate-user-emacs-file "var/package-quickstart.el"))
(when (file-newer-than-file-p package-user-dir package-quickstart-file)
  (delete-file package-quickstart-file)
  (delete-file (concat package-quickstart-file "c")))
(package-activate-all)
(when (memq 'gruber-material-dark package-activated-list)
  (condition-case err
      (load-theme 'gruber-material-dark-intense :no-confirm)
    (error (message "Failed to load theme: %s" (error-message-string err)))))

(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(let ((load-prefer-newer t))
  (require 'adh-startup)
  (require 'adh-options))

(setq default-frame-alist
      `((fullscreen . maximized)
        (undecorated . ,(not adh-window-decoration))
        (,(if (memq system-type '(windows-nt darwin)) 'alpha 'alpha-background)
         . ,adh-frame-opacity)
        ,@default-frame-alist))
(set-face-attribute 'default nil :family adh-mono-spaced-font :height adh-mono-spaced-font-size)
(set-face-attribute 'fixed-pitch nil :family adh-mono-spaced-font)
