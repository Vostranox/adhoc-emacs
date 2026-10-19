;;; -*- lexical-binding: t; coding: utf-8 -*-

(when (version< emacs-version "31")
  (error "[adh][error] The configuration assumes Emacs 31 or newer (found %s)" emacs-version))

(defvar adh-use-custom-keybinds)

(defcustom adh-auto-compile-config nil
  "When non-nil, check and rebuild stale compiled configuration after startup.
Compilation runs in a background Emacs and takes effect next session.
When nil, use `adh-compile-config' to check and compile on demand."
  :group 'adhoc
  :type 'boolean)

(defvar adh--init-errors-p nil
  "Non-nil if any adhoc loading errors occurred during initialization.")

(defvar adh--init-error-count 0
  "Number of initialization errors, including errors caught by `use-package'.")

(defconst adh-layout-modules
  '(adh-core-packages adh-project adh-flycheck adh-ext-packages
    adh-completion adh-minibuffer adh-settings adh-consult adh-magit adh-jump)
  "Modules whose commands the custom keybindings bind.")

(defun adh--record-package-error (type _message &optional level _buffer-name)
  "Record error-level `use-package' diagnostics of TYPE and LEVEL.
Keep the original warning visible, including errors from deferred configuration."
  (when (and (eq (if (consp type) (car type) type) 'use-package)
             (memq level '(:error :emergency)))
    (setq adh--init-errors-p t
          adh--init-error-count (1+ adh--init-error-count))))

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
ERROR-TYPE, a string or a function returning one, describes signaled
failures.  Return nil on failure, including error diagnostics caught
internally by `use-package'."
  (let ((before adh--init-error-count))
    (condition-case err
        (when (and (funcall function) (= before adh--init-error-count))
          (message "[adh][ok] %s %s" action target)
          t)
      (error (adh--log-init-error
              (if (functionp error-type) (funcall error-type) error-type)
              target err)))))

(defun adh--stale-source (feature)
  "Return FEATURE's source in lisp/ if its byte-code is missing or older."
  (let ((source (locate-user-emacs-file (format "lisp/%s.el" feature))))
    (and (file-newer-than-file-p source (concat source "c")) source)))

(defun adh-require! (feature)
  "Require FEATURE, logging success; catch and record any load error.
A module whose byte-code is missing or stale loads from its source."
  (adh--call-with-init-report
   (lambda () (require feature (adh--stale-source feature))) "Required"
   (lambda ()
     (format "Error loading feature (file: %s)"
             (or (locate-library (symbol-name feature))
                 "Path not found in load-path")))
   feature))

(defun adh-load! (file)
  "Load FILE from the user dir, logging errors; return t if loaded."
  (adh--call-with-init-report
   (lambda () (load (locate-user-emacs-file file) t))
   "Loaded" "Failed to load file" file))

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

(defun adh--modules ()
  "Return the configuration modules in lisp/ as absolute file names."
  (directory-files (locate-user-emacs-file "lisp/") t "\\`adh-.*\\.el\\'"))

(defun adh--config-stale-p ()
  "Return non-nil if a module's .elc is missing or out of date.
It is out of date when it is older than its .el, elpa/ or the Emacs binary."
  (let ((emacs (expand-file-name invocation-name invocation-directory)))
    (seq-some (lambda (source)
                (let ((compiled (concat source "c")))
                  (or (file-newer-than-file-p source compiled)
                      (file-newer-than-file-p package-user-dir compiled)
                      (file-newer-than-file-p emacs compiled))))
              (adh--modules))))

(defun adh--package-quickstart-stale-p ()
  "Return non-nil if the package quickstart files need rebuilding."
  (or (not (file-exists-p package-quickstart-file))
      (file-newer-than-file-p package-user-dir package-quickstart-file)
      (file-newer-than-file-p package-quickstart-file (concat package-quickstart-file "c"))))

(defun adh-compile-config (&optional force)
  "Recompile lisp/ and the package quickstart for the next session.
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
  "Start a background batch Emacs that runs `adh--compile-batch'."
  (require 'adh-package-config)
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
                "-l" ,(expand-file-name "adh-startup.el" dir)
                "-f" "adh--compile-batch")
     :sentinel (lambda (proc _)
                 (unless (process-live-p proc)
                   (if (zerop (process-exit-status proc))
                       (message "[adh] Compiled lisp/ and the package quickstart")
                     (display-warning 'adhoc "[adh][warning] Compiling the configuration failed; see buffer \"*adh-compile-config*\"")))))))

(defun adh--quickstart-defer-info ()
  "Make the package quickstart load `info' only once something needs it.
`package-quickstart-refresh' ends the file with a form that requires `info'
at startup just to extend `Info-directory-list'; run it after `info' loads."
  (with-temp-buffer
    (insert-file-contents package-quickstart-file)
    (when (re-search-forward "^(progn[ \t\n]+(require 'info)" nil t)
      (goto-char (match-beginning 0))
      (let ((start (point))
            (form (read (current-buffer)))
            (print-length nil)
            (print-level nil)
            (coding-system-for-write 'emacs-internal))
        (delete-region start (point))
        (prin1 `(with-eval-after-load 'info ,@(cddr form)) (current-buffer))
        (write-region nil nil package-quickstart-file nil 'silent)
        (byte-compile-file package-quickstart-file)))))

(defun adh--compile-batch ()
  "Compile lisp/ and refresh the package quickstart in this batch Emacs.
Modules are also native-compiled here, in one session rather than by
per-file JIT subprocesses, so names that macros generate stay unique."
  (require 'bytecomp)
  (require 'package)
  (package-initialize 'no-activate)
  (dolist (package package-alist)
    (package-activate (car package)))
  (eval native-comp-async-env-modifier-form t)
  (let ((modules (adh--modules))
        (byte-compile-warnings '(not unresolved)))
    (message "[adh] Compiling lisp/...")
    (dolist (module modules)
      (with-demoted-errors "[adh][warning] Removing old byte-code: %S"
        (delete-file (concat module "c"))))
    (dolist (module modules)
      (unless (byte-compile-file module)
        (error "[adh] Compiling %s failed" module)))
    (when adh--init-errors-p
      (error "[adh] Compiling lisp/ reported package errors"))
    (when (native-comp-available-p)
      (message "[adh] Native-compiling lisp/...")
      (let ((byte-compile-warnings nil))
        (dolist (module modules)
          (with-demoted-errors "[adh][warning] Native compilation failed: %S"
            (native-compile module))))))
  (message "[adh] Building package quickstart...")
  (make-directory (file-name-directory package-quickstart-file) t)
  (let ((warning-inhibit-types '((bytecomp))))
    (unless (package-quickstart-refresh)
      (error "[adh] Compiling package quickstart failed"))
    (adh--quickstart-defer-info))
  (message "[adh] Configuration build finished"))

(defun adh--maybe-compile-config ()
  "Check and rebuild the configuration after startup when opted in."
  (when (and adh-auto-compile-config (not adh--init-errors-p))
    (adh-compile-config)))

(advice-add 'display-warning :before #'adh--record-package-error)

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

(setq native-comp-async-env-modifier-form
      '(progn
         (setq transient-save-history nil)
         (require 'no-littering nil t)))

(add-hook 'emacs-startup-hook #'adh--maybe-compile-config)

(provide 'adh-startup)

;;; adh-startup.el ends here
