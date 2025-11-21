;;; -*- lexical-binding: t; coding: utf-8 -*-

(defvar adh--segment-modal-state-alist
  '((normal . ("N" . font-lock-variable-name-face))
    (insert . ("I" . font-lock-string-face))
    (motion . ("G" . font-lock-variable-name-face))))

(setq-default mode-line-position
  '((-3 "%p") " "
    (line-number-mode ("(%l"))
    (column-number-mode (", %c)"))))

(setq-default mode-line-mule-info
              '((:eval
                 (let ((eol (coding-system-eol-type buffer-file-coding-system))
                       (encoding (format-mode-line " %z")))
                   (concat encoding
                           (pcase eol
                             (0 ":")
                             (1 "\\")
                             (2 "/")
                             (_ "")))))))

(defun adh--mode-line-buffer-id ()
  (let ((face (if (mode-line-window-selected-p)
                  'mode-line-buffer-id
                'mode-line-buffer-id-inactive)))
    (propertize "%b"
                'face face
                'mouse-face 'mode-line-highlight
                'help-echo "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                'local-map mode-line-buffer-identification-keymap)))

(defun adh--segment-modal-fn ()
  (when (boundp 'meow--current-state)
    (let* ((mode-cons (alist-get meow--current-state
                                 adh--segment-modal-state-alist))
           (label (car-safe mode-cons))
           (face (cdr-safe mode-cons)))
      (when label
        (propertize label 'face face)))))

(defun adh--segment-eglot ()
  (when (and (fboundp 'eglot-managed-p)
             (eglot-managed-p)
             (boundp 'eglot--mode-line-format))
    (list "(" eglot--mode-line-format ")")))

(defun adh--segment-flymake ()
  (when (and (bound-and-true-p flymake-mode)
             (boundp 'flymake-mode-line-format)
             flymake-mode-line-format)
    (let ((s (string-trim (format-mode-line flymake-mode-line-format))))
      (unless (string-empty-p s) s))))

(defun adh--segment-tab ()
  (let ((tabs (tab-bar-tabs)))
    (when (> (length tabs) 1)
      (let ((current-name (alist-get 'name (tab-bar--current-tab))))
        (list
         "("
         (mapconcat
          (lambda (tab)
            (let ((name (alist-get 'name tab)))
              (propertize name
                          'face (if (string= name current-name)
                                    'font-lock-constant-face
                                  'font-lock-variable-name-face))))
          tabs
          " ")
         ") ")))))

(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                (:propertize ("" mode-line-mule-info mode-line-modified mode-line-remote))
                "  "
                (:eval (adh--mode-line-buffer-id))
                "    "
                (:eval
                 (when (mode-line-window-selected-p)
                   (list
                    mode-line-position
                    "    "
                    (let ((m (adh--segment-modal-fn)))
                      (when m (format "(%s) " m)))
                    mode-line-modes
                    (adh--segment-tab)
                    (adh--segment-eglot)
                    " "
                    (adh--segment-flymake))))))

(provide 'adh-modeline)
