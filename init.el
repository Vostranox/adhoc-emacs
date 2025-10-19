;;; -*- lexical-binding: t; coding: utf-8 -*-

(when (version< emacs-version "30")
  (error "[adhoc] Configuration requires Emacs 30 or newer (found %s)." emacs-version))

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(require 'adh-start-buffer)
(require 'adh-functions)
(require 'adh-settings)

(require 'adh-use-package-bootstrap)
(require 'adh-core-packages)
(require 'adh-project)
(require 'adh-eglot)

(require 'adh-ext-packages)
(require 'adh-completion)
(require 'adh-minibuffer)
(require 'adh-prog-modes)
(require 'adh-consult)
(require 'adh-perspective)
(require 'adh-magit)
(require 'adh-meow)

(require 'adh-dashboard)
(require 'adh-modeline)

(require 'adh-keybinds)

(require 'adh-server)
