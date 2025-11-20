;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh--consult-fd-with-region (&optional i)
  (if (use-region-p)
      (consult-fd i (buffer-substring-no-properties (region-beginning) (region-end)))
    (consult-fd i)))

(defun adh--consult-fd-directories (&optional arg)
  (let ((consult-fd-args (concat adh--consult-fd-args " -t directory --prune")))
    (consult-fd arg)))

(defun adh--consult-ripgrep-with-region (&optional i)
  (if (use-region-p)
      (consult-ripgrep i (buffer-substring-no-properties (region-beginning) (region-end)))
    (consult-ripgrep i)))

(defun adh-consult-fd-here ()
  (interactive)
  (adh--consult-fd-with-region default-directory))

(defun adh-consult-fd-directories-here ()
  (interactive)
  (adh--consult-fd-directories default-directory))

(defun adh-consult-ripgrep-here ()
  (interactive)
  (adh--consult-ripgrep-with-region default-directory))

(defun adh-consult-fd-project ()
  (interactive)
  (adh--consult-fd-with-region (adh--get-project-dir)))

(defun adh-consult-fd-directories-project ()
  (interactive)
  (adh--consult-fd-directories (adh--get-project-dir)))

(defun adh-consult-ripgrep-project ()
  (interactive)
  (adh--consult-ripgrep-with-region (adh--get-project-dir)))

(defun adh-consult-fd-all ()
  (interactive)
  (adh--consult-fd-with-region 1))

(defun adh-consult-ripgrep-all ()
  (interactive)
  (adh--consult-ripgrep-with-region 1))

(defun adh-consult-line-with-region ()
  (interactive)
  (let ((consult-preview-key 'any))
    (if (use-region-p)
        (let ((input (buffer-substring-no-properties (region-beginning) (region-end))))
          (deactivate-mark)
          (consult-line input))
      (consult-line))))

(defun adh-consult-imenu ()
  (interactive)
  (let ((consult-preview-key 'any))
    (consult-imenu)))

(defun adh-consult-goto-line ()
  (interactive)
  (let ((consult-preview-key 'any))
    (consult-goto-line)))

(defun adh-consult-select-window ()
  (interactive)
  (require 'consult)
  (let* ((all-wins (cdr (window-list)))
         (wins (seq-remove (lambda (w)
                             (window-parameter w 'no-other-window))
                           all-wins)))
    (cond
     ((= (length wins) 0)
      (call-interactively #'other-window))
     ((= (length wins) 1)
      (select-window (car wins)))
     (t
      (let* ((cands (mapcar (lambda (w)
                              (cons (buffer-name (window-buffer w)) w))
                            wins))
             (choice (consult--read (mapcar #'car cands)
                                    :prompt "Window: "
                                    :require-match t
                                    :sort nil)))
        (when choice
          (select-window (cdr (assoc choice cands)))))))))

(defun adh-consult-to-all ()
  (interactive)
  (setq unread-command-events
        (append (listify-key-sequence
                 (kbd (concat consult-narrow-key " " "r")))
                unread-command-events)))

(use-package consult
  :ensure t :after embark
  :custom
  (consult-buffer-filter "\\*")
  (consult-narrow-key "C-,")
  (consult-preview-key "C-SPC")
  (consult-line-start-from-top t)
  :config
  (defconst adh--fd-executable-path (locate-user-emacs-file (concat "opt/fd/bin/fd" (when (eq system-type 'windows-nt) ".exe"))))
  (defconst adh--consult-fd-args  (concat adh--fd-executable-path " --sort-by-depth --full-path --hidden --no-ignore --color=never --exclude .git"))

  (setq consult-fd-args (concat adh--consult-fd-args " -t file"))
  (setq consult-ripgrep-args (concat consult-ripgrep-args " -P --hidden --no-ignore -g !.git -g !TAGS"))

  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files/Everything")
    (setq consult-locate-args '("es.exe" "-s" "-full-path-and-name")))

  (consult-customize consult--source-buffer :name "λ" :hidden nil :narrow ?r
                     consult--source-recent-file :name "λ" :hidden t :narrow ?r
                     consult--source-hidden-buffer :name "◆" :hidden t :default nil :narrow ?\s
                     consult--source-buffer-register :hidden t :default nil :narrow nil
                     consult--source-file-register :hidden t :default nil :narrow nil
                     consult--source-bookmark :hidden t :default nil :narrow nil)

  (define-advice consult-narrow (:around (orig-fun &rest args) adh--consult-overlay)
    (prog1 (apply orig-fun args)
      (when consult--narrow-overlay
        (let* ((label (alist-get consult--narrow
                                 (plist-get consult--narrow-config :keys)))
               (text (format "%s" label))
               (prop (propertize (concat " " text)
                                 'face 'consult-narrow-indicator)))
          (overlay-put consult--narrow-overlay 'before-string prop)))))
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark
  :ensure t
  :custom
  (embark-indicators
   '(embark-minimal-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  (embark-prompter 'embark-keymap-prompter)
  :hook
  (embark-after-export . (lambda ()
                           (let ((bn (buffer-name)))
                             (when (string-match "\\*Embark Export: .* - \\(.*\\)\\*" bn)
                               (let ((search-input (match-string 1 bn)))
                                 (rename-buffer (format "*e: %s: %s" (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)) search-input) t)
                                 (adh-to-side-window)))))))

(use-package embark-consult
  :ensure t :after (consult embark))

(provide 'adh-consult)
