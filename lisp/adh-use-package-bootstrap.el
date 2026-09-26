;;; -*- lexical-binding: t; coding: utf-8 -*-

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(require 'use-package)

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
