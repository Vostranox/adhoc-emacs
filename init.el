;;; -*- lexical-binding: t; coding: utf-8 -*-

(when (version< emacs-version "30")
  (warn "[adh] The configuration assumes Emacs 30 or newer (found %s)." emacs-version))

(defmacro adh-require! (feature)
  `(condition-case err
       (and
        (require ,feature) t)
     (error
      (warn "[adh] Error loading %S: %s" ,feature (error-message-string err))
      (message "[adh] Error loading %S: %s" ,feature (error-message-string err))
      nil)))

(defmacro adh-load! (filename)
  `(condition-case err
       (progn
         (when (load (locate-user-emacs-file ,filename) t)
           (message "[adh] Loaded %s" ,filename))
         nil)
     (error
      (warn "[adh] Failed to load %s: %s" ,filename (error-message-string err))
      (message "[adh] Failed to load %s: %s" ,filename (error-message-string err))
      nil)))

(defgroup adhoc nil
  "AdHoc Emacs group"
  :group 'applications)

(defcustom adh-completion-backend 'company
  "Completion backend"
  :group 'adhoc
  :type '(choice (const :tag "Corfu" corfu)
                 (const :tag "Company" company)))

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(adh-load! "adh-custom-pre-init.el")

(adh-require! 'adh-functions)
(adh-require! 'adh-use-package-bootstrap)
(adh-require! 'adh-emacs)
(adh-require! 'adh-core-packages)
(adh-require! 'adh-project)
(adh-require! 'adh-eglot)
(adh-require! 'adh-ext-packages)
(adh-require! 'adh-completion)
(adh-require! 'adh-minibuffer)
(adh-require! 'adh-prog-modes)
(adh-require! 'adh-consult)
(adh-require! 'adh-magit)
(adh-require! 'adh-meow)
(adh-require! 'adh-modeline)
(adh-require! 'adh-keybinds)
(adh-require! 'adh-server)

(adh-load! "adh-custom-post-init.el")
