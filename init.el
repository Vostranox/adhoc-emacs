;;; -*- lexical-binding: t; coding: utf-8 -*-

(when (version< emacs-version "30")
  (error "[adh][error] The configuration assumes Emacs 30 or newer (found %s)." emacs-version))

(defvar adh--init-errors-p nil
  "Non-nil if any adhoc loading errors occurred during initialization.")

(defun adh--log-init-error (type target err)
  "Log an init failure."
  (let ((msg (format "[adh][error] %s %S: %s" type target (error-message-string err))))
    (message msg)
    (display-warning 'adhoc msg :error)
    (setq adh--init-errors-p t)
    nil))

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (and adh--init-errors-p (get-buffer "*Messages*"))
              (select-window (window-main-window))
              (dolist (win (window-list))
                (set-window-dedicated-p win nil)
                (set-window-parameter win 'no-delete-other-windows nil)
                (set-window-parameter win 'window-side nil))
              (switch-to-buffer "*Messages*")
              (delete-other-windows)
              (message "[adh][error] Initialization failed with errors. Review *Messages* and *Warnings*."))))

(defmacro adh-require! (feature)
  "Require FEATURE, logging success; catch and record any load error."
  `(let ((feat ,feature))
     (condition-case err
         (progn (require feat)
                (message "[adh][ok] Required %s" feat)
                t)
       (error
        (adh--log-init-error
         (format "Error loading feature (file: %s)"
                 (or (locate-library (symbol-name feat))
                     "Path not found in load-path"))
         feat err)))))

(defmacro adh-load! (filename)
  "Load FILENAME from the user dir; a missing file is fine, errors are logged.
Returns t when the file was loaded, nil if absent or on error."
  `(let ((file ,filename))
     (condition-case err
         (when (load (locate-user-emacs-file file) t)
           (message "[adh][ok] Loaded %s" file)
           t)
       (error (adh--log-init-error "Failed to load file" file err)))))

(add-to-list 'load-path (locate-user-emacs-file "lisp"))

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
