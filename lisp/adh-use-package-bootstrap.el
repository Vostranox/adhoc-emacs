;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-package-config)
(require 'use-package)

(defun adh-upgrade-packages (&optional query)
  "Upgrade all packages, including those installed with package-vc.
When QUERY is non-nil, ask before upgrading archive packages."
  (interactive (list t))
  (package-upgrade-all query)
  (package-vc-upgrade-all))

(defun adh--with-vc-git (fn &rest args)
  "Call FN with ARGS with the Git VC backend enabled."
  (let ((vc-handled-backends (if (memq 'Git vc-handled-backends)
                                 vc-handled-backends
                               (cons 'Git vc-handled-backends))))
    (apply fn args)))

(dolist (fn '(package-vc-install package-vc-checkout
              package-vc-upgrade package-vc--unpack-1))
  (advice-add fn :around #'adh--with-vc-git))
(when (native-comp-available-p)
  (setq package-native-compile t)
  (setq native-comp-async-report-warnings-errors 'silent))

(use-package no-littering
  :ensure t :demand t)

(provide 'adh-use-package-bootstrap)

;;; adh-use-package-bootstrap.el ends here
