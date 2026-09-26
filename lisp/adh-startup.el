;;; -*- lexical-binding: t; coding: utf-8 -*-

(when (version< emacs-version "31")
  (error "[adh][error] The configuration assumes Emacs 31 or newer (found %s)." emacs-version))

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

(setq native-comp-async-env-modifier-form
      '(progn
         (setq transient-save-history nil)
         (require 'no-littering nil t)))

(defun adh--config-stale-p ()
  "Return non-nil when some file under lisp/ has no .elc or is newer than it."
  (catch 'stale
    (dolist (el (directory-files (locate-user-emacs-file "lisp/") t "\\`adh-.*\\.el\\'"))
      (when (file-newer-than-file-p el (concat el "c"))
        (throw 'stale t)))))

(defun adh-compile-config ()
  "Byte-compile every file under lisp/ in a separate Emacs and wait for it.
All files are recompiled together, so none keeps a stale expansion of
another's macro.  Loading the new .elc files queues their native compilation.
Return non-nil on success."
  (interactive)
  (let* ((dir (locate-user-emacs-file "lisp/"))
         (buf (get-buffer-create " *adh-compile-config*"))
         (form `(progn
                  (setq package-user-dir ,package-user-dir)
                  (when (fboundp 'startup-redirect-eln-cache)
                    (startup-redirect-eln-cache ,(car native-comp-eln-load-path)))
                  (package-activate-all)
                  (require 'no-littering nil t)
                  (add-to-list 'load-path ,dir)
                  (let ((failed 0))
                    (dolist (el (directory-files ,dir t "\\`adh-.*\\.el\\'"))
                      (unless (byte-compile-file el)
                        (setq failed (1+ failed))))
                    (setq kill-emacs-hook nil)
                    (kill-emacs failed)))))
    (message "[adh] Compiling lisp/...")
    (with-current-buffer buf (erase-buffer))
    (let ((proc (make-process
                 :name "adh-compile-config"
                 :buffer buf
                 :noquery t
                 :command (list (expand-file-name invocation-name invocation-directory)
                                "-Q" "--batch" "--eval" (prin1-to-string form)))))
      (while (process-live-p proc)
        (if (input-pending-p)
            (accept-process-output proc 0.05)
          (sit-for 0.05)))
      (if (eq 0 (process-exit-status proc))
          (progn (message "[adh] Compiling lisp/...done") t)
        (adh--log-init-error "Failed to compile" dir
                             (list 'error (format "see buffer %S" (buffer-name buf))))))))

(provide 'adh-startup)
