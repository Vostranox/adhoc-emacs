;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(require 'use-package)
(setq use-package-verbose nil)

(defun adh--with-vc-git (fn &rest args)
  "Call FN with ARGS with the Git VC backend enabled."
  (let ((vc-handled-backends (if (memq 'Git vc-handled-backends)
                                 vc-handled-backends
                               (cons 'Git vc-handled-backends))))
    (apply fn args)))

(dolist (fn '(package-vc-install package-vc-install-from-checkout
              package-vc-upgrade package-vc-rebuild))
  (advice-add fn :around #'adh--with-vc-git))
(when (native-comp-available-p)
  (setq package-native-compile t)
  (setq native-comp-async-report-warnings-errors 'silent))

(use-package no-littering
  :ensure t :demand t)

(provide 'adh-use-package-bootstrap)
