;;; -*- lexical-binding: t; coding: utf-8 -*-

;; A trimmed magit-status that shows only the staging-relevant sections.
(define-derived-mode magit-staging-mode magit-status-mode "magit-staging"
  "Like `magit-status-mode' but limited to staged/unstaged changes."
  :group 'magit-status)

(defun magit-staging-refresh-buffer ()
  "Populate a `magit-staging-mode' buffer with just the change sections."
  (magit-insert-section (status)
    (magit-insert-status-headers)
    (magit-insert-unstaged-changes)
    (magit-insert-staged-changes)))

(defun adh--magit-show-commit-current-file (orig-fun &rest args)
  "Advice for `magit-show-commit': when blaming, limit the diff to the current file."
  (if (and (null (nth 2 args))
           (or (bound-and-true-p magit-blame-mode)
               (bound-and-true-p magit-blame-read-only-mode))
           (buffer-file-name))
      (let* ((rel (magit-file-relative-name (buffer-file-name)))
             (rev (nth 0 args))
             (cmdargs (nth 1 args))
             (module (nth 3 args)))
        (funcall orig-fun rev cmdargs (list rel) module))
    (apply orig-fun args)))

(defun adh-magit-staging ()
  "Open the trimmed staging-only magit buffer."
  (interactive)
  (require 'magit)
  (magit-setup-buffer #'magit-staging-mode #'magit-staging-refresh-buffer))

(defun adh-magit-staging-quick ()
  "Show an existing staging buffer if one is hidden, else open a fresh one.
With a prefix arg, always open a fresh one."
  (interactive)
  (require 'magit)
  (if-let* ((buffer
            (and (not current-prefix-arg)
                 (not (magit-get-mode-buffer 'magit-staging-mode nil 'selected))
                 (magit-get-mode-buffer 'magit-staging-mode))))
      (magit-display-buffer buffer)
    (adh-magit-staging)))

(defun adh-magit-show-commit-original ()
  "Show the full commit at point, bypassing the blame file-narrowing advice."
  (interactive)
  (let ((had (advice-member-p #'adh--magit-show-commit-current-file
                              'magit-show-commit)))
    (unwind-protect
        (progn
          (when had
            (advice-remove 'magit-show-commit #'adh--magit-show-commit-current-file))
          (call-interactively #'magit-show-commit))
      (when had
        (advice-add 'magit-show-commit :around #'adh--magit-show-commit-current-file)))))

(defun adh-magit-restore-current ()
  "Discard uncommitted changes in the current file (git restore), then revert it."
  (interactive)
  (require 'magit)
  (let ((path (adh-copy-full-path)))
    (when (y-or-n-p (format "Restore changes in %s? " path))
      (magit-call-git "restore" path)
      (when (get-file-buffer path)
        (with-current-buffer (get-file-buffer path)
          (revert-buffer :ignore-auto :noconfirm))))))

(defun adh-magit-switch-or-status ()
  "Switch to an open magit buffer if any exist, otherwise open `magit-status'."
  (interactive)
  (let ((magit-buffers (magit-mode-get-buffers)))
    (if magit-buffers
        (call-interactively #'magit-switch-to-repository-buffer-other-window)
      (magit-status-quick))))

(defun adh-magit-visit-thing-other-window ()
  "Visit the thing at point in another window.
Dispatches to whatever RET would visit for the section at point
\(file, commit, stash, branch, ...) but displays it in another window."
  (interactive)
  (let ((cmd (key-binding (kbd "RET"))))
    (if (eq cmd #'magit-diff-visit-file)
        (call-interactively #'magit-diff-visit-file-other-window)
      (let ((display-buffer-overriding-action '(nil (inhibit-same-window . t))))
        (call-interactively (or cmd #'magit-visit-thing))))))

(defun adh-magit-preview-thing ()
  "Visit the thing at point in another window but keep point in this window."
  (interactive)
  (save-selected-window
    (adh-magit-visit-thing-other-window)))

(defun adh-magit-blame-copy-short-hash ()
  "Copy the 7-character short hash of the blamed commit to the kill ring."
  (interactive)
  (magit-blame-copy-hash)
  (when-let* ((last (current-kill 0)))
    (kill-new (substring last 0 7))
    (message "%s" (substring last 0 7))))

(defun adh-toggle-magit-blame ()
  "Toggle `magit-blame' for the current file."
  (interactive)
  (if (bound-and-true-p magit-blame-mode)
      (magit-blame-mode 0)
    (call-interactively 'magit-blame-addition)))

(defun adh-magit-log-buffer-file-follow ()
  "Show the log for the current file, following it across renames."
  (interactive)
  (magit-log-buffer-file t))

(defun adh-magit-log-trace-region-or-line ()
  "Show the line-history (git log -L) of the region, or the current line."
  (interactive)
  (require 'magit)
  (let* ((file (or (magit-file-relative-name)
                   (user-error "Buffer is not visiting a file")))
         (commit (or magit-buffer-revision
                     (magit-get-current-branch)
                     "HEAD"))
         (beg (if (use-region-p) (line-number-at-pos (region-beginning)) (line-number-at-pos)))
         (end (if (use-region-p) (line-number-at-pos (region-end)) (line-number-at-pos)))
         (l-arg (format "-L%s,%s:%s" beg end file))
         (raw-args (magit-log-arguments))
         (clean-args (cl-remove-if (lambda (arg)
                                     (or (not (stringp arg))
                                         (string-prefix-p "-L" arg)))
                                   (flatten-tree raw-args))))
    (magit-log-setup-buffer
     (list commit)
     (cons l-arg clean-args)
     nil
     nil)))

(use-package magit
  :ensure t :defer 10
  :commands (magit magit-status magit-status-quick)
  :init
  (setq magit-auto-revert-mode nil)
  :custom
  (magit-refresh-verbose t)
  (magit-commit-show-diff nil)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk t)
  (magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18))
  (magit-section-initial-visibility-alist
   '((staged . hide) (unstaged . hide) (untracked . hide) (stashes . hide) (unpushed . hide) (unpulled . hide)))
  :config
  (put 'magit-log-mode 'magit-log-default-arguments '("-n1024"))
  (advice-add #'magit-show-commit :around #'adh--magit-show-commit-current-file)
  :hook
  (magit-mode . (lambda () (let ((bn (buffer-name)))
                             (when (string-match "^magit\\(.*\\): \\(.*\\)" bn)
                               (rename-buffer (format "*m%s: %s" (match-string 1 bn) (match-string 2 bn)) t))))))

(provide 'adh-magit)
