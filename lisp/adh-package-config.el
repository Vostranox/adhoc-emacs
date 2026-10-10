;;; -*- lexical-binding: t; coding: utf-8 -*-

(defvar package-archives)

(with-eval-after-load 'package
  (unless (assoc "melpa" package-archives)
    (setq package-archives
          (append package-archives '(("melpa" . "https://melpa.org/packages/"))))))

(provide 'adh-package-config)

;;; adh-package-config.el ends here
