;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(require 'use-package)
(setq use-package-verbose nil)
(when (native-comp-available-p)
  (setq package-native-compile t)
  (setq native-comp-async-report-warnings-errors 'silent))

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name "var/eln-cache/" user-emacs-directory)))

(use-package no-littering
  :ensure t)

(provide 'adh-use-package-bootstrap)
