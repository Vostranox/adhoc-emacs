;;; -*- lexical-binding: t; coding: utf-8 -*-

(when (version< emacs-version "31")
  (error "[adh][error] The configuration assumes Emacs 31 or newer (found %s)." emacs-version))

(defcustom adh-auto-compile-config nil
  "When non-nil, check and rebuild stale compiled configuration after startup.
Compilation runs in a background Emacs and takes effect next session.
When nil, use `adh-compile-config' to check and compile on demand."
  :group 'adhoc
  :type 'boolean)

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
  "Return non-nil if generated init.el or init.elc needs rebuilding."
  (let ((init (locate-user-emacs-file "init.el"))
        (compiled (locate-user-emacs-file "init.elc"))
        (config (locate-user-emacs-file "config.el")))
    (or (not (file-exists-p init))
        (not (file-exists-p config))
        (seq-some (lambda (file) (file-newer-than-file-p file compiled))
                  (append (list init config package-user-dir
                                (expand-file-name invocation-name invocation-directory))
                          (directory-files (locate-user-emacs-file "lisp/") t "\\`adh-.*\\.el\\'"))))))

(defun adh--package-quickstart-stale-p ()
  "Return non-nil if the package quickstart files need rebuilding."
  (or (not (file-exists-p package-quickstart-file))
      (file-newer-than-file-p package-user-dir package-quickstart-file)
      (file-newer-than-file-p package-quickstart-file (concat package-quickstart-file "c"))))

(defun adh-compile-config (&optional force)
  "Rebuild generated init.el, init.elc and package quickstart for next session.
With a prefix argument FORCE, rebuild even when everything is up to date.
Return the background build process, or nil if no rebuild is needed.
If a build is already running, return that process."
  (interactive "P")
  (let ((proc (get-process "adh-compile-config")))
    (cond
     ((and proc (process-live-p proc))
      (when (called-interactively-p 'interactive)
        (message "[adh] Configuration compilation is already running"))
      proc)
     ((or force (adh--config-stale-p) (adh--package-quickstart-stale-p))
      (adh--compile-config))
     (t
      (when (called-interactively-p 'interactive)
        (message "[adh] Compiled configuration is up to date"))
      nil))))

(defun adh--compile-config ()
  "Start a background build without removing the previous working init."
  (let ((dir (locate-user-emacs-file "lisp/")))
    (make-process
     :name "adh-compile-config"
     :buffer (with-current-buffer (get-buffer-create " *adh-compile-config*")
               (erase-buffer)
               (current-buffer))
     :noquery t
     :command `(,(expand-file-name invocation-name invocation-directory)
                "-Q" "--batch" "-L" ,dir
                "--eval" ,(prin1-to-string
                           `(setq user-emacs-directory ,user-emacs-directory
                                  package-user-dir ,package-user-dir
                                  package-quickstart-file ,package-quickstart-file
                                  native-comp-jit-compilation nil))
                ,@(when (native-comp-available-p)
                    (list "--eval" (format "(startup-redirect-eln-cache %S)"
                                           (car native-comp-eln-load-path))))
                "-l" ,(expand-file-name "adh-build.el" dir)
                "-f" "adh--build-config")
     :sentinel (lambda (proc _)
                 (unless (process-live-p proc)
                   (if (zerop (process-exit-status proc))
                       (message "[adh] Built init.el and init.elc")
                     (display-warning 'adhoc "[adh][warning] Building init failed; see buffer \" *adh-compile-config*\"")))))))

(defun adh--maybe-compile-config ()
  "Check and rebuild the configuration after startup when opted in."
  (when (and adh-auto-compile-config (not adh--init-errors-p))
    (adh-compile-config)))

(add-hook 'emacs-startup-hook #'adh--maybe-compile-config)

(provide 'adh-startup)
