;;; -*- lexical-binding: t; coding: utf-8 -*-

(adh-require! 'adh-vars)

(adh-load! "adh-custom-pre-init.el")

(adh-require! 'adh-functions)
(adh-require! 'adh-use-package-bootstrap)
(adh-require! 'adh-emacs)
(adh-require! 'adh-core-packages)
(adh-require! 'adh-project)
(adh-require! 'adh-eglot)
(adh-require! 'adh-ext-packages)
(adh-require! 'adh-completion)
(adh-require! 'adh-setup)
(adh-require! 'adh-minibuffer)
(adh-require! 'adh-prog-modes)
(adh-require! 'adh-consult)
(adh-require! 'adh-magit)
(adh-require! 'adh-modeline)
(when adh-use-custom-keybinds
  (adh-require! 'adh-meow)
  (adh-require! 'adh-keybinds))
(adh-require! 'adh-server)

(adh-load! "adh-custom-post-init.el")
