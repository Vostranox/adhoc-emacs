;;; -*- lexical-binding: t; coding: utf-8 -*-

(defvar adh--segment-modal-state-alist
  '((normal . ("N" . font-lock-variable-name-face))
    (insert . ("I" . font-lock-string-face))
    (motion . ("G" . font-lock-variable-name-face))))

(defun adh--segment-modal-fn ()
  (when (boundp 'meow--current-state)
    (let* ((mode-cons (alist-get meow--current-state
                                 adh--segment-modal-state-alist))
           (label     (car-safe mode-cons))
           (face      (cdr-safe mode-cons)))
      (when label
        (propertize label 'face face)))))

(defun adh--mode-line-buffer-id ()
  (let ((face (if (mode-line-window-selected-p)
                  'mode-line-buffer-id
                'mode-line-buffer-id-inactive)))
    (propertize "%b"
                'face face
                'mouse-face 'mode-line-highlight
                'help-echo "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                'local-map mode-line-buffer-identification-keymap)))

(defun adh--segment-flymake ()
  (when (and (bound-and-true-p flymake-mode)
             (boundp 'flymake-mode-line-format)
             flymake-mode-line-format)
    (let ((s (string-trim (format-mode-line flymake-mode-line-format))))
      (unless (string-empty-p s) s))))

(setq-default mode-line-position
              '((line-number-mode ("(%l"))
                (column-number-mode (", %c)"))))

(setq-default mode-line-mule-info
              '((:eval
                 (let ((eol (coding-system-eol-type buffer-file-coding-system))
                       (encoding (format-mode-line " %z")))
                   (concat encoding
                           (pcase eol
                             (0 ":") (1 "\\") (2 "/") (_ "")))))))

(defun adh--segment-eglot ()
  (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (if (eq system-type 'darwin)
        (when (boundp 'eglot-mode-line-format)
          (let* ((rendered
                  (cl-loop for e in eglot-mode-line-format
                           for r = (format-mode-line e)
                           unless (string-empty-p r)
                           collect (cons r (eq e 'eglot-mode-line-menu))))
                 (str
                  (cl-loop for (rspec . rest) on rendered
                           for (r . titlep) = rspec
                           concat r
                           when rest concat (if titlep ":" "/"))))
            (when (and str (not (string-empty-p str)))
              (list " (" str ")"))))
      (when (boundp 'eglot--mode-line-format)
        (list " (" eglot--mode-line-format ")")))))

(defun adh--segment-persp ()
  (when (fboundp 'persp-mode-line)
    (persp-mode-line)))

(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                (:propertize ("" mode-line-mule-info mode-line-modified mode-line-remote))
                "  "
                (:eval (adh--mode-line-buffer-id))
                "  "
                (:eval
                 (when (mode-line-window-selected-p)
                   (list
                    mode-line-position
                    "  "
                    (let ((m (adh--segment-modal-fn)))
                      (when m (format "(%s)" m)))
                    " "
                    mode-line-modes
                    (adh--segment-persp)
                    (adh--segment-eglot)
                    " "
                    (adh--segment-flymake))))))

(provide 'adh-modeline)
