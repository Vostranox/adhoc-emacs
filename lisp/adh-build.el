;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'cl-lib)
(require 'bytecomp)
(require 'pp)
(require 'adh-startup (locate-user-emacs-file "lisp/adh-startup.el"))
(require 'adh-package-config (locate-user-emacs-file "lisp/adh-package-config.el"))
(require 'package)

(defun adh--read-config-forms (file)
  "Read the Lisp forms in FILE, signalling malformed input."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (let (forms)
      (while (progn (forward-comment (point-max)) (not (eobp)))
        (push (read (current-buffer)) forms))
      (nreverse forms))))

(defun adh--defined-functions (form)
  "Return the functions that FORM, or any form within it, defines."
  (pcase form
    (`(,(or 'quote 'function) . ,_) nil)
    (`(,(or 'defun 'defsubst 'cl-defun 'define-minor-mode 'define-derived-mode)
       ,(and (pred symbolp) name) . ,_)
     (list (list name)))
    (`(define-globalized-minor-mode ,(and (pred symbolp) name) . ,_)
     (list (list name) (cons (intern (format "%s-enable-in-buffer" name)) t)))
    (`(defalias ',(and (pred symbolp) name) . ,_) (list (list name)))
    (`(define-advice ,(and (pred symbolp) symbol) (,_ ,_ ,(and (pred symbolp) name) . ,_) . ,_)
     (list (cons (intern (format "%s@%s" symbol name)) t)))
    ((pred proper-list-p) (mapcan #'adh--defined-functions form))))

(defun adh--module-form (form)
  "Return the module's top-level FORM as init.el needs it."
  (pcase form
    (`(declare-function ,function ,(and (pred stringp) (pred (string-prefix-p "adh-")) file)
                        . ,rest)
     `(declare-function ,function ,(format "lisp/%s.el" (file-name-sans-extension file))
                        ,@rest))
    (_ form)))

(defun adh--generate-init (target)
  "Expand config.el's module loads into the source file TARGET."
  (let (dependencies declarations)
    (cl-labels
        ((expand
          (form)
          (cond
           ((eq (car-safe form) 'adh-require!)
            (pcase form
              (`(adh-require! ',(and (pred symbolp) feature))
               (let* ((file (locate-user-emacs-file (format "lisp/%s.el" feature)))
                      (body (mapcar #'adh--module-form (adh--read-config-forms file))))
                 (dolist (item body)
                   (pcase (car-safe item)
                     ((or 'require 'eval-when-compile 'eval-and-compile)
                      (push (pcase item
                              (`(require ',(and (pred symbolp) dependency))
                               (if (string-prefix-p "adh-" (symbol-name dependency))
                                   `(require ',dependency
                                             (locate-user-emacs-file
                                              ,(format "lisp/%s.el" dependency)))
                                 item))
                              (_ item))
                            dependencies))
                     ((or 'defvar 'defvar-local 'defconst 'defcustom 'defvar-keymap
                          'define-minor-mode 'define-globalized-minor-mode)
                      (push `(defvar ,(cadr item)) declarations)))
                   (pcase-dolist (`(,function . ,generated) (adh--defined-functions item))
                     (push `(declare-function ,function ,(format "lisp/%s.el" feature)
                                              ,@(and generated '(t t)))
                           declarations)))
                 `(adh--call-with-init-report
                   (lambda ()
                     (unless (featurep ',feature)
                       (with-suppressed-warnings ((make-local nil)) ,@body))
                     t)
                   "Required" "Error loading bundled feature" ',feature)))
              (_ (error "Expected (adh-require! 'FEATURE), got %S" form))))
           ((or (not (consp form)) (memq (car form) '(quote function))) form)
           (t (mapcar #'expand form)))))
      (let ((forms (mapcar #'expand
                           (adh--read-config-forms (locate-user-emacs-file "config.el")))))
        (let ((coding-system-for-write 'utf-8-unix)
              (print-length nil)
              (print-level nil)
              (pp-default-function
               (lambda (form)
                 (let ((start (point-marker))
                       (end (copy-marker (point) t)))
                   (cl-letf (((get 'condition-case 'lisp-indent-function) 1))
                     (pp-fill form))
                   (unless (equal (car (read-from-string (buffer-substring-no-properties start end)))
                                  form)
                     (delete-region start end)
                     (goto-char start)
                     (pp-29 form)))))
              (pp-escape-newlines nil))
          (with-temp-file target
            (insert ";;; -*- lexical-binding: t; coding: utf-8 -*-\n"
                    ";; Generated from config.el and lisp/ by adh-compile-config.\n"
                    ";; Edit those sources, then rebuild; do not edit this file.\n\n")
            (dolist (form (delete-dups (nreverse declarations)))
              (pp form (current-buffer)))
            (pp `(eval-when-compile
                   (when (bound-and-true-p byte-compile-current-file)
                     ,@(delete-dups (nreverse dependencies))))
                (current-buffer))
            (dolist (form forms)
              (insert "\n")
              (pp form (current-buffer)))
            (emacs-lisp-mode)
            (goto-char (point-min))
            (while (search-forward "\t" nil t)
              (when (nth 3 (syntax-ppss))
                (replace-match "\\t" t t)))
            (untabify (point-min) (point-max))))))))

(defun adh--publish-config (files)
  "Replace generated FILES, restoring their old contents if a replacement fails.
FILES is an alist of temporary source paths and final destination paths."
  (let (backups installed complete)
    (unwind-protect
        (progn
          (dolist (file files)
            (when (file-exists-p (cdr file))
              (let ((backup (make-temp-file (concat (cdr file) ".backup-"))))
                (push (cons (cdr file) backup) backups)
                (copy-file (cdr file) backup t t))))
          (dolist (file files)
            (rename-file (car file) (cdr file) t)
            (push (cdr file) installed))
          (setq complete t))
      (unless complete
        (dolist (file installed)
          (if-let* ((backup (cdr (assoc file backups))))
              (rename-file backup file t)
            (delete-file file))))
      (dolist (backup backups)
        (when (file-exists-p (cdr backup))
          (delete-file (cdr backup)))))))

(defun adh--build-config ()
  "Generate and compile init.el and package quickstart in a fresh batch Emacs."
  (let* ((init (locate-user-emacs-file "init.el"))
         (quickstart package-quickstart-file)
         (adh--init-errors-p nil)
         (adh--init-error-count 0)
         staged-init staged-quickstart)
    (make-directory (file-name-directory quickstart) t)
    (unwind-protect
        (progn
          (setq staged-init (make-temp-file (concat init ".build-") nil ".el")
                staged-quickstart (make-temp-file (concat quickstart ".build-") nil ".el"))
          (message "[adh] Generating init.el...")
          (adh--generate-init staged-init)
          (package-initialize 'no-activate)
          (dolist (package package-alist)
            (package-activate (car package)))
          (eval native-comp-async-env-modifier-form t)
          (message "[adh] Compiling init.el...")
          (unless (let ((byte-compile-warnings '(not noruntime)))
                    (byte-compile-file staged-init))
            (error "[adh] Compiling generated init.el failed"))
          (when adh--init-errors-p
            (error "[adh] Compiling generated init.el reported package errors"))
          (message "[adh] Building package quickstart...")
          (let ((package-quickstart-file staged-quickstart)
                (warning-inhibit-types '((bytecomp))))
            (unless (package-quickstart-refresh)
              (error "[adh] Compiling package quickstart failed")))
          (when adh--init-errors-p
            (error "[adh] Building package quickstart reported package errors"))
          (message "[adh] Publishing compiled configuration...")
          (adh--publish-config
           (list (cons staged-quickstart quickstart)
                 (cons (concat staged-quickstart "c") (concat quickstart "c"))
                 (cons staged-init init)
                 (cons (concat staged-init "c") (concat init "c"))))
          (dolist (file (directory-files (locate-user-emacs-file "lisp/") t "\\`adh-.*\\.elc\\'"))
            (with-demoted-errors "[adh][warning] Removing old byte-code: %S"
              (delete-file file)))
          (message "[adh] Configuration build finished"))
      (dolist (file (list staged-init staged-quickstart))
        (when file
          (dolist (path (list file (concat file "c")))
            (when (file-exists-p path)
              (delete-file path))))))))

(provide 'adh-build)

;;; adh-build.el ends here
