;;; -*- lexical-binding: t; coding: utf-8 -*-

(when (version< emacs-version "31")
  (error "[adh][error] The configuration assumes Emacs 31 or newer (found %s)." emacs-version))

(defvar adh--init-errors-p nil
  "Non-nil if any adhoc loading errors occurred during initialization.")

(defun adh--log-init-error (type target err)
  "Log an init failure."
  (let ((msg (format "[adh][error] %s %S: %s" type target (error-message-string err))))
    (message "%s" msg)
    (display-warning 'adhoc msg :error)
    (setq adh--init-errors-p t)
    nil))

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (and adh--init-errors-p (get-buffer "*Messages*"))
              (let ((main (window-main-window)))
                (select-window (if (window-live-p main) main (frame-first-window))))
              (dolist (win (window-list))
                (set-window-dedicated-p win nil)
                (set-window-parameter win 'no-delete-other-windows nil)
                (set-window-parameter win 'window-side nil))
              (switch-to-buffer "*Messages*")
              (delete-other-windows)
              (message "[adh][error] Initialization failed with errors. Review *Messages* and *Warnings*."))))

(defun adh-require! (feature)
  "Require FEATURE, logging success; catch and record any load error."
  (condition-case err
      (progn (require feature)
             (message "[adh][ok] Required %s" feature)
             t)
    (error
     (adh--log-init-error
      (format "Error loading feature (file: %s)"
              (or (locate-library (symbol-name feature))
                  "Path not found in load-path"))
      feature err))))

(defun adh-load! (file)
  "Load FILE from the user dir, logging errors; return t if loaded."
  (condition-case err
      (when (load (locate-user-emacs-file file) t)
        (message "[adh][ok] Loaded %s" file)
        t)
    (error (adh--log-init-error "Failed to load file" file err))))

(defun adh-keybinds-need! (features)
  "Return non-nil if FEATURES are loaded; otherwise report the keybindings as off."
  (let ((missing (seq-remove #'featurep features)))
    (when missing
      (message "[adh] Custom keybindings off; they need %s"
               (mapconcat #'symbol-name missing ", ")))
    (null missing)))

(setq native-comp-async-env-modifier-form
      '(progn
         (setq transient-save-history nil)
         (require 'no-littering nil t)))

(defun adh--config-stale-p ()
  "Return non-nil if a lisp/ .elc is missing or older than its .el, elpa/ or Emacs."
  (let ((emacs (expand-file-name invocation-name invocation-directory)))
    (catch 'stale
      (dolist (el (directory-files (locate-user-emacs-file "lisp/") t "\\`adh-.*\\.el\\'"))
        (let ((elc (concat el "c")))
          (when (or (file-newer-than-file-p el elc)
                    (file-newer-than-file-p package-user-dir elc)
                    (file-newer-than-file-p emacs elc))
            (throw 'stale t)))))))

(defun adh--delete-config-elc ()
  "Delete lisp/*.elc, so lisp/ loads from source."
  (dolist (elc (directory-files (locate-user-emacs-file "lisp/") t "\\.elc\\'"))
    (delete-file elc)))

(defun adh-compile-config ()
  "Byte-compile lisp/ in a background Emacs, for the next session."
  (interactive)
  (adh--delete-config-elc)
  (let ((dir (locate-user-emacs-file "lisp/")))
    (make-process
     :name "adh-compile-config"
     :buffer (with-current-buffer (get-buffer-create " *adh-compile-config*")
               (erase-buffer)
               (current-buffer))
     :noquery t
     :command `(,(expand-file-name invocation-name invocation-directory)
                "-Q" "--batch" "-L" ,dir
                "--eval" ,(format "(setq package-user-dir %S)" package-user-dir)
                ,@(when (native-comp-available-p)
                    (list "--eval" (format "(startup-redirect-eln-cache %S)"
                                           (car native-comp-eln-load-path))))
                "-f" "package-activate-all"
                "--eval" ,(prin1-to-string native-comp-async-env-modifier-form)
                "--eval" ,(prin1-to-string
                           `(with-demoted-errors "[adh][error] Package quickstart: %S"
                              (require 'package)
                              (setq package-quickstart-file ,package-quickstart-file)
                              (make-directory (file-name-directory package-quickstart-file) t)
                              (let ((warning-inhibit-types '((bytecomp))))
                                (package-quickstart-refresh))))
                "-f" "batch-byte-compile"
                ,@(directory-files dir t "\\`adh-.*\\.el\\'"))
     :sentinel (lambda (proc _)
                 (unless (process-live-p proc)
                   (if (zerop (process-exit-status proc))
                       (message "[adh] Compiled lisp/")
                     (display-warning 'adhoc "[adh][warning] Compiling lisp/ failed; see buffer \" *adh-compile-config*\"")))))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (and (not adh--init-errors-p)
                       (or (adh--config-stale-p) (not (file-exists-p package-quickstart-file))))
              (adh-compile-config))))

(provide 'adh-startup)
