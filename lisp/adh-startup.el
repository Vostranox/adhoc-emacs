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
  "Load FILENAME from the user dir, logging errors; return t if loaded."
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
  "Return non-nil if a lisp/ file has no .elc or is newer than it."
  (catch 'stale
    (dolist (el (directory-files (locate-user-emacs-file "lisp/") t "\\`adh-.*\\.el\\'"))
      (when (file-newer-than-file-p el (concat el "c"))
        (throw 'stale t)))))

(defun adh-compile-config ()
  "Byte-compile all of lisp/ in a child Emacs; return non-nil on success.
Everything recompiles together so no file keeps a stale macro expansion."
  (interactive)
  (let* ((dir (locate-user-emacs-file "lisp/"))
         (buf (get-buffer-create " *adh-compile-config*"))
         (form `(progn
                  (setq package-user-dir ,package-user-dir)
                  (when (fboundp 'startup-redirect-eln-cache)
                    (startup-redirect-eln-cache ,(car native-comp-eln-load-path)))
                  (package-activate-all)
                  (require 'no-littering nil t)
                  (require 'use-package)
                  (setq load-prefer-newer t)
                  (add-to-list 'load-path ,dir)
                  (let ((files (directory-files ,dir t "\\`adh-.*\\.el\\'"))
                        (failed 0)
                        missing)
                    (setq use-package-ensure-function
                          (lambda (name args _state &optional _)
                            (dolist (ensure args)
                              (let ((pkg (if (eq ensure t) name ensure)))
                                (when (consp pkg) (setq pkg (car pkg)))
                                (when (and pkg (not (package-installed-p pkg)))
                                  (push (bare-symbol pkg) missing))))))
                    (advice-add 'use-package-vc-install :override
                                (lambda (arg &optional _)
                                  (unless (package-installed-p (car arg))
                                    (push (bare-symbol (car arg)) missing))))
                    (dolist (el files)
                      (unless (byte-compile-file el)
                        (setq failed (1+ failed))))
                    (setq kill-emacs-hook nil)
                    (when missing
                      (dolist (el files)
                        (when (file-exists-p (concat el "c"))
                          (delete-file (concat el "c"))))
                      (princ (format "\nadh-missing: %S\n" (delete-dups missing)))
                      (kill-emacs 2))
                    (kill-emacs failed)))))
    (message "[adh] Compiling lisp/...")
    (with-current-buffer buf (erase-buffer))
    (let* ((default-directory dir)
           (proc (make-process
                 :name "adh-compile-config"
                 :buffer buf
                 :noquery t
                 :command (list (expand-file-name invocation-name invocation-directory)
                                "-Q" "--batch" "--eval" (prin1-to-string form)))))
      (while (process-live-p proc)
        (if (input-pending-p)
            (accept-process-output proc 0.05)
          (sit-for 0.05)))
      (pcase (process-exit-status proc)
        (0 (message "[adh] Compiling lisp/...done") t)
        (2 (message "[adh] Missing packages %s; running lisp/ from source"
                    (with-current-buffer buf
                      (goto-char (point-max))
                      (and (re-search-backward "^adh-missing: \\(.*\\)$" nil t)
                           (match-string 1))))
           nil)
        (_ (adh--log-init-error "Failed to compile" dir
                                (list 'error (format "see buffer %S" (buffer-name buf)))))))))

(provide 'adh-startup)
