;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-functions)

(define-advice register-val-jump-to (:around (orig val arg) adh-no-file-query-prompt)
  (if (and (consp val) (eq (car val) 'file-query))
      (cl-letf (((symbol-function 'y-or-n-p) #'always))
        (funcall orig val arg))
    (funcall orig val arg)))

(defun adh--rename-isearch-occur-buffer (&rest _)
  "Name the `isearch-occur' buffer after the search string."
  (when (get-buffer "*Occur*")
    (with-current-buffer "*Occur*"
      (rename-buffer (format "*s-occur: %s*" isearch-string) t))))

(defun adh--isearch-with-region (forward)
  "Start isearch in FORWARD direction, seeded with the region."
  (if (use-region-p)
      (let ((search-string (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (isearch-mode forward)
        (isearch-yank-string search-string))
    (call-interactively (if forward #'isearch-forward #'isearch-backward))))

(defun adh-isearch-forward-with-region ()
  "Search forward, starting from the active region if any."
  (interactive)
  (adh--isearch-with-region t))

(defun adh-isearch-backward-with-region ()
  "Search backward, starting from the active region if any."
  (interactive)
  (adh--isearch-with-region nil))

(defun adh--apply-vc (on)
  "Turn the built-in VC backends and `global-diff-hl-mode' ON or off."
  (if on
      (progn
        (setq vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Git Hg))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (and buffer-file-name
                       (not (file-remote-p buffer-file-name)))
              (ignore-errors (vc-refresh-state)))))
        (when (require 'diff-hl nil t)
          (global-diff-hl-mode 1)))
    (setq vc-handled-backends nil)
    (when (bound-and-true-p global-diff-hl-mode)
      (global-diff-hl-mode -1))))

(defun adh-switch-dired-buffer ()
  "Switch to a Dired buffer."
  (interactive)
  (adh-switch-buffer-of-mode 'dired-mode "Dired: "))

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (isearch-case-fold-search t)
  (search-whitespace-regexp ".*?")
  (lazy-count-prefix-format "(%s/%s) ")
  :config
  (advice-add 'isearch-occur :after #'adh--rename-isearch-occur-buffer))

(defun adh--dired-rename-buffer ()
  "Name a Dired buffer PROJECT/REL/ inside a project, else by its path."
  (unless (bound-and-true-p dirvish-fd-buffer)
    (let ((root (adh--get-project-dir)))
      (rename-buffer
       (if root
           (let ((rel (file-relative-name default-directory root)))
             (concat (file-name-nondirectory (directory-file-name root)) "/"
                     (unless (equal rel "./") rel)))
         (abbreviate-file-name default-directory))
       t))))

(use-package dired
  :ensure nil :defer t
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh --group-directories-first --sort=version")
  :hook
  (dired-mode . adh--dired-rename-buffer)
  (dired-mode . (lambda () (display-line-numbers-mode -1))))

(use-package wdired
  :ensure nil :defer t
  :custom
  (wdired-allow-to-change-permissions t))

(use-package org
  :ensure nil
  :custom
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-indent-indentation-per-level 4)
  (org-directory (expand-file-name "org-tasks" user-emacs-directory))
  (org-capture-templates `(("t" "Todo" entry (file ,(expand-file-name "tasks.org" org-directory)) "* TODO %?  %^g")))
  (org-agenda-files (list org-directory))
  (org-agenda-skip-unavailable-files t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-deadline-warning-days 0)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  :hook
  (org-mode . org-indent-mode))

(use-package savehist
  :init
  (put 'command-history 'history-length 100)
  :config
  (savehist-mode 1)
  (setq command-history (seq-take command-history 100)))

(use-package recentf
  :functions recentf-cleanup
  :custom
  (recentf-exclude '("^/tmp"))
  (recentf-max-saved-items 5000)
  (recentf-auto-cleanup 'never)
  (recentf-show-messages nil)
  :config
  (recentf-mode 1)
  (add-hook 'after-init-hook
            (lambda ()
              (run-with-idle-timer 0 nil (lambda () (let ((inhibit-message t)) (recentf-cleanup)))))))

(add-to-list 'minor-mode-alist '(adh-use-vc " vc"))

(use-package transient :defer t)

(use-package repeat
  :ensure nil
  :config
  (repeat-mode 1))

(use-package glasses
  :ensure nil :defer t
  :custom
  (glasses-separate-parentheses-p nil))

(define-globalized-minor-mode adh-global-glasses-mode glasses-mode glasses-mode
  :predicate '(prog-mode) :group 'adhoc)

(defun adh--apply-subwords (on)
  "Turn subword motion and display ON or off in all buffers."
  (cond (on (global-subword-mode 1)
            (adh-global-glasses-mode 1))
        (t (when (bound-and-true-p global-subword-mode)
             (global-subword-mode -1))
           (when adh-global-glasses-mode
             (adh-global-glasses-mode -1)))))

(defun adh--apply-which-key (on)
  "Turn `which-key-mode' ON or off; inside a transient menu, once it exits."
  (remove-hook 'transient-exit-hook 'transient--resume-which-key-mode)
  (cond ((and on (bound-and-true-p transient--prefix))
         (add-hook 'transient-exit-hook 'transient--resume-which-key-mode))
        (on (which-key-mode 1))
        ((bound-and-true-p which-key-mode) (which-key-mode -1))))

(use-package ediff
  :ensure nil :defer t
  :custom
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :hook
  (ediff-quit . (lambda () (global-whitespace-mode 1)))
  (ediff-startup . (lambda () (global-whitespace-mode -1) (when (fboundp 'meow-insert-mode) (meow-insert-mode 1)))))

(use-package diff-mode
  :ensure nil :defer t
  :custom
  (diff-font-lock-prettify t)
  :hook
  (diff-mode . (lambda () (setq-local show-trailing-whitespace t))))

(use-package which-key
  :ensure nil :defer t
  :custom
  (which-key-popup-type 'minibuffer)
  (which-key-lighter nil))

(adh--apply-vc adh-use-vc)
(adh--apply-subwords adh-subwords)
(adh--apply-which-key adh-use-which-key)

(provide 'adh-core-packages)
