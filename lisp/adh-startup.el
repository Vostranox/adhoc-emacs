;;; -*- lexical-binding: t; coding: utf-8 -*-

(when (version< emacs-version "31")
  (error "[adh][error] The configuration assumes Emacs 31 or newer (found %s)" emacs-version))

(defcustom adh-auto-compile-config nil
  "When non-nil, check and rebuild stale compiled configuration after startup.
Compilation runs in a background Emacs and takes effect next session.
When nil, use `adh-compile-config' to check and compile on demand."
  :group 'adhoc
  :type 'boolean)

(defvar adh-use-custom-keybinds)

(defvar adh--init-errors-p nil
  "Non-nil if any adhoc loading errors occurred during initialization.")

(defvar adh--init-error-count 0
  "Number of initialization errors, including errors caught by `use-package'.")

(defun adh--record-package-error (type _message &optional level _buffer-name)
  "Record error-level `use-package' diagnostics of TYPE and LEVEL.
Keep the original warning visible, including errors from deferred configuration."
  (when (and (eq (if (consp type) (car type) type) 'use-package)
             (memq level '(:error :emergency)))
    (setq adh--init-errors-p t
          adh--init-error-count (1+ adh--init-error-count))))

(advice-add 'display-warning :before #'adh--record-package-error)

(defun adh--log-init-error (type target err)
  "Log an initialization failure of TYPE for TARGET using error data ERR.
Return nil and mark initialization as failed."
  (let ((msg (format "[adh][error] %s %S: %s" type target (error-message-string err))))
    (message "%s" msg)
    (display-warning 'adhoc msg :error)
    (setq adh--init-errors-p t
          adh--init-error-count (1+ adh--init-error-count))
    nil))

(defun adh--call-with-init-report (function action error-type target)
  "Call FUNCTION and report ACTION for TARGET if it succeeds without errors.
ERROR-TYPE describes signaled failures.  Return nil on failure, including
error diagnostics caught internally by `use-package'."
  (let ((before adh--init-error-count))
    (condition-case err
        (when (and (funcall function) (= before adh--init-error-count))
          (message "[adh][ok] %s %s" action target)
          t)
      (error (adh--log-init-error error-type target err)))))

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
  (adh--call-with-init-report
   (lambda () (require feature)) "Required"
   (format "Error loading feature (file: %s)"
           (or (locate-library (symbol-name feature))
               "Path not found in load-path"))
   feature))

(defun adh-load! (file)
  "Load FILE from the user dir, logging errors; return t if loaded."
  (adh--call-with-init-report
   (lambda () (load (locate-user-emacs-file file) t))
   "Loaded" "Failed to load file" file))

(defconst adh-layout-modules
  '(adh-core-packages adh-project adh-flycheck adh-ext-packages
    adh-completion adh-minibuffer adh-settings adh-consult adh-magit)
  "Modules whose commands the custom keybindings bind.")

(defun adh-layout-ready-p ()
  "Return non-nil when the custom keybindings may load; say why otherwise."
  (when adh-use-custom-keybinds
    (if adh--init-errors-p
        (progn
          (message "[adh] Custom keybindings off; configuration loading reported errors")
          nil)
      (let ((missing (seq-remove #'featurep adh-layout-modules)))
        (when missing
          (message "[adh] Custom keybindings off; they need %s"
                   (mapconcat #'symbol-name missing ", ")))
        (null missing)))))

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
When called interactively, show the build log as it is produced.
Return the background build process, or nil if no rebuild is needed.
If a build is already running, return that process."
  (interactive "P")
  (let ((proc (get-process "adh-compile-config")))
    (cond
     ((and proc (process-live-p proc))
      (when (called-interactively-p 'interactive)
        (message "[adh] Configuration compilation is already running")))
     ((or force (adh--config-stale-p) (adh--package-quickstart-stale-p))
      (setq proc (adh--compile-config)))
     (t
      (setq proc nil)
      (when (called-interactively-p 'interactive)
        (message "[adh] Compiled configuration is up to date"))))
    (when (and proc (called-interactively-p 'interactive))
      (display-buffer (process-buffer proc)))
    proc))

(defun adh--compile-config ()
  "Start a background build without removing the previous working init."
  (require 'adh-package-config (locate-user-emacs-file "lisp/adh-package-config.el"))
  (require 'package)
  (let ((dir (locate-user-emacs-file "lisp/")))
    (make-process
     :name "adh-compile-config"
     :buffer (with-current-buffer (get-buffer-create "*adh-compile-config*")
               (let ((inhibit-read-only t))
                 (erase-buffer))
               (special-mode)
               (setq-local window-point-insertion-type t)
               (let ((inhibit-read-only t))
                 (insert "[adh] Starting configuration build...\n"))
               (current-buffer))
     :connection-type 'pipe
     :noquery t
     :command `(,(expand-file-name invocation-name invocation-directory)
                "-Q" "--batch" "-L" ,dir
                "--eval" ,(prin1-to-string
                           `(setq user-emacs-directory ,user-emacs-directory
                                  package-user-dir ,package-user-dir
                                  package-quickstart-file ,package-quickstart-file
                                  package-archives ',package-archives
                                  package-archive-priorities ',package-archive-priorities
                                  package-pinned-packages ',package-pinned-packages
                                  package-load-list ',package-load-list
                                  package-directory-list ',package-directory-list
                                  package-install-upgrade-built-in ,package-install-upgrade-built-in
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
                     (display-warning 'adhoc "[adh][warning] Building init failed; see buffer \"*adh-compile-config*\"")))))))

(defun adh--maybe-compile-config ()
  "Check and rebuild the configuration after startup when opted in."
  (when (and adh-auto-compile-config (not adh--init-errors-p))
    (adh-compile-config)))

(add-hook 'emacs-startup-hook #'adh--maybe-compile-config)

(provide 'adh-startup)

;;; adh-startup.el ends here
