;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-options)
(require 'adh-functions)

(defvar embark-exporters-alist)
(defvar vr/engine)

(defun adh--zoxide-add ()
  "Add `default-directory' to zoxide if it is installed."
  (when (executable-find "zoxide")
    (zoxide-add)))

(use-package zoxide
  :ensure t
  :hook
  ((find-file dired-mode) . adh--zoxide-add))

(defun adh--apply-use-dirvish (on)
  "Open new Dired buffers in Dirvish when ON, else in plain Dired."
  (when (and on (not (fboundp 'dirvish-override-dired-mode)))
    (user-error "Dirvish is not installed"))
  (cond (on (dirvish-override-dired-mode 1))
        ((bound-and-true-p dirvish-override-dired-mode)
         (dirvish-override-dired-mode -1))))

(defun adh-switch-dired-dwim ()
  "Jump through Dirvish history if in use, else switch to a Dired buffer."
  (interactive)
  (if (and adh-use-dirvish (fboundp 'dirvish-history-jump))
      (call-interactively #'dirvish-history-jump)
    (adh-switch-buffer-of-mode 'dired-mode "Dired: ")))

(use-package dirvish
  :vc (:url "https://github.com/Vostranox/dirvish" :rev :newest)
  :defer t
  :custom
  (dirvish-use-header-line nil)
  (dirvish-hide-cursor nil)
  (dirvish-use-mode-line nil)
  (dirvish-attributes '(collapse))
  (dirvish-hide-details nil)
  (dirvish-preview-dispatchers '(video image gif archive font pdf))
  (dirvish-cache-dir (no-littering-expand-var-file-name "dirvish/"))
  (dirvish-wdired-cursor nil)
  (dirvish-preview-other-window nil)
  (dirvish-fd-search-icon ":")
  (dirvish-fd-switches "--full-path --hidden --no-ignore --exclude .git --path-separator=/")
  (dirvish-fd-program (if (file-executable-p adh--fd-program) adh--fd-program (executable-find "fd")))
  (dirvish-yank-keys '(("c" "Copy here" dirvish-yank)
                       ("r" "Move here" dirvish-move)
                       ("s" "Make symlinks here" dirvish-symlink)
                       ("y" "Make relative symlinks here" dirvish-relative-symlink)
                       ("h" "Make hardlinks here" dirvish-hardlink)))
  :init
  (with-eval-after-load 'dired
    (with-demoted-errors "[adh] %S" (adh--apply-use-dirvish adh-use-dirvish)))
  (with-eval-after-load 'embark
    (when (fboundp 'dirvish-embark-export)
      (setf (alist-get 'file embark-exporters-alist) #'dirvish-embark-export))))

(defun adh-mc-keyboard-quit-dwim ()
  "Exit multiple-cursors if active, otherwise `adh-keyboard-quit-dwim'."
  (interactive)
  (if (bound-and-true-p multiple-cursors-mode)
      (mc/keyboard-quit)
    (adh-keyboard-quit-dwim)))

(defun adh-isearch-mc-mark-all ()
  "Exit isearch and put a cursor on each visible match, with the match selected."
  (interactive)
  (unless isearch-mode
    (user-error "Not in isearch"))
  (require 'multiple-cursors-core)
  (let ((forward isearch-forward)
        (start (min (point) (or isearch-other-end (point))))
        matches)
    (unless (string-empty-p isearch-string)
      (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search isearch-case-fold-search)
              (isearch-forward t)
              (search-invisible nil))
          (while (and (< (point) (point-max))
                      (ignore-error search-failed
                        (isearch-search-string isearch-string (point-max) t)))
            (let ((beg (match-beginning 0))
                  (end (match-end 0)))
              (cond ((= beg end) (unless (eobp) (forward-char 1)))
                    ((funcall isearch-filter-predicate beg end)
                     (push (cons beg end) matches))))))))
    (unless matches
      (user-error "No visible match for %S" isearch-string))
    (setq matches (nreverse matches))
    (let ((current (or (assoc start matches)
                       (seq-find (lambda (m) (> (cdr m) start)) matches)
                       (car (last matches)))))
      (isearch-done)
      (push-mark nil t)
      (isearch-clean-overlays)
      (mc/remove-fake-cursors)
      (dolist (m (append (remq current matches) (list current)))
        (set-mark (if forward (car m) (cdr m)))
        (goto-char (if forward (cdr m) (car m)))
        (unless (eq m current)
          (mc/create-fake-cursor-at-point))))
    (mc/maybe-multiple-cursors-mode)))

(use-package multiple-cursors
  :ensure t :defer t
  :custom
  (mc/always-run-for-all t)
  :init
  (with-eval-after-load 'multiple-cursors-core
    (add-to-list 'mc--default-cmds-to-run-once #'adh-isearch-mc-mark-all)))

(defun adh-avy-goto-line-indent ()
  "Jump to a line with avy and land on its first non-blank character."
  (interactive)
  (avy-goto-line)
  (back-to-indentation))

(use-package avy
  :ensure t :defer t
  :custom
  (avy-background t)
  (avy-keys '(?n ?r ?t ?s ?g ?y ?h ?a ?e ?i ?l ?d ?c ?f ?o ?u)))

(use-package yasnippet
  :ensure t :defer t
  :init
  (setq yas-verbosity 0)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets :ensure t :defer t)

(use-package visual-regexp
  :ensure t :defer t
  :custom
  (vr/default-regexp-modifiers '(:I t :M t :S nil)))

(defun adh--vr-with-windows-shell (fn &rest args)
  "Call FN with ARGS using cmdproxy for Visual Regexp's Python engine on Windows."
  (if (and (eq system-type 'windows-nt) (eq vr/engine 'python))
      (let ((shell-file-name (expand-file-name "cmdproxy.exe" exec-directory))
            (shell-command-switch "/c"))
        (apply fn args))
    (apply fn args)))

(use-package visual-regexp-steroids
  :ensure t :after visual-regexp
  :config
  (when (and (not (executable-find "python")) (executable-find "python3"))
    (setq vr/command-python (replace-regexp-in-string "\\`python " "python3 " vr/command-python)))
  ;; Wrap the package's legacy advice so quoting and execution share the shell.
  (dolist (fn '(vr--feedback-function vr--get-replacements))
    (advice-add fn :around #'adh--vr-with-windows-shell '((depth . -100)))))

(use-package vundo
  :ensure t :defer t
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package wgrep
  :ensure t :defer t
  :custom
  (wgrep-auto-save-buffer t))

(use-package rainbow-mode
  :ensure t :defer t
  :custom
  (rainbow-x-colors nil))

(use-package sudo-edit
  :ensure t :defer t)

(use-package diff-hl
  :ensure t :defer t
  :config
  (define-advice diff-hl-show-hunk-inline-show (:filter-return (overlay) adh-window-local)
    (when (overlayp overlay)
      (overlay-put overlay 'window (selected-window)))
    overlay)
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(defvar popper-open-popup-alist)
(defvar popper-popup-status)

(defun adh--popup-buffer-p (buf)
  "Return non-nil if BUF matches an entry of `adh-popup-buffers'."
  (with-current-buffer buf
    (seq-some (lambda (entry)
                (if (stringp entry)
                    (string-match-p entry (buffer-name))
                  (derived-mode-p entry)))
              adh-popup-buffers)))

(defun adh--popper-display (buffer &optional alist)
  "Show popup BUFFER where visible, else at the bottom, and select it.
ALIST supplies action options as in `display-buffer'."
  (save-current-buffer
    (let ((win (or (display-buffer-reuse-window buffer alist)
                   (popper-display-popup-at-bottom
                    buffer (append alist '((window-parameters . ((no-other-window . t)))))))))
      (when (window-parameter win 'window-side)
        (adh--popper-window-height win))
      (select-window win))))

(defun adh-select-popup ()
  "Select the open popup, or reopen the last one."
  (interactive)
  (if-let* ((win (caar popper-open-popup-alist)))
      (select-window win)
    (popper-toggle)))

(defun adh-popup-toggle-type ()
  "Turn the popup into a bottom window, or the current buffer into a popup."
  (interactive)
  (let ((display-buffer-overriding-action
         (if (memq popper-popup-status '(popup user-popup))
             `(display-buffer-at-bottom (window-height . ,(window-total-height)))
           display-buffer-overriding-action)))
    (popper-toggle-type)))

(use-package popper
  :ensure t :demand t
  :custom
  (popper-reference-buffers '(adh--popup-buffer-p))
  (popper-display-function #'adh--popper-display)
  (popper-window-height #'adh--popper-window-height)
  (popper-mode-line "")
  :config
  (put 'popper-popup-status 'permanent-local t)
  (popper-mode 1))

(use-package string-inflection
  :ensure t :defer t)

(use-package gruber-material-dark
  :vc (:url "https://github.com/Vostranox/gruber-material-dark" :rev :newest)
  :demand t
  :config
  (unless (custom-theme-enabled-p 'gruber-material-dark-intense)
    (load-theme 'gruber-material-dark-intense :no-confirm)))

(provide 'adh-ext-packages)

;;; adh-ext-packages.el ends here
