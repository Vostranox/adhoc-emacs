;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-functions)

(eval-when-compile
  (when (bound-and-true-p byte-compile-current-file)
    (require 'magit)))

(defvar adh--magit-submodule-foreach-history nil
  "Minibuffer history of `adh-magit-submodule-foreach'.")

(define-derived-mode magit-staging-mode magit-status-mode "magit-staging"
  "Like `magit-status-mode' but limited to staged/unstaged changes."
  :group 'magit-status)

(defun magit-staging-refresh-buffer ()
  "Populate a `magit-staging-mode' buffer with just the change sections."
  (magit-insert-section (status)
    (magit-insert-status-headers)
    (magit-insert-unstaged-changes)
    (magit-insert-staged-changes)))

(defvar adh--magit-show-full-commit nil
  "Non-nil while `adh-magit-show-commit-original' runs.")

(defun adh--magit-show-commit-current-file (fn rev &optional args files module)
  "Advice for `magit-show-commit': limit the diff to the blamed file."
  (when-let* ((chunk (and (bound-and-true-p magit-blame-mode) (magit-current-blame-chunk))))
    (setq files (unless adh--magit-show-full-commit (list (oref chunk orig-file)))))
  (funcall fn rev args files module))

(defun adh-magit-staging ()
  "Open the trimmed staging-only magit buffer."
  (interactive)
  (require 'magit)
  (magit-setup-buffer #'magit-staging-mode))

(defun adh-magit-staging-quick ()
  "Show a hidden staging buffer or open one; a prefix arg forces a new one."
  (interactive)
  (require 'magit)
  (if-let* ((buffer
            (and (not current-prefix-arg)
                 (not (magit-get-mode-buffer 'magit-staging-mode nil 'selected))
                 (magit-get-mode-buffer 'magit-staging-mode))))
      (magit-display-buffer buffer)
    (adh-magit-staging)))

(defun adh-magit-show-commit-original ()
  "Show the full commit at point, bypassing the blame narrowing."
  (interactive)
  (let ((adh--magit-show-full-commit t))
    (call-interactively #'magit-show-commit)))

(defun adh-magit-restore-current ()
  "Discard unstaged changes to this file or directory (git restore)."
  (interactive)
  (require 'magit)
  (let ((path (expand-file-name (or (adh--buffer-file-name) default-directory))))
    (when (y-or-n-p (format "Restore %s? "
                            (file-relative-name path (magit-toplevel (file-name-directory path)))))
      (magit-call-git "restore" path)
      (when-let* ((buf (get-file-buffer path)))
        (with-current-buffer buf
          (revert-buffer :ignore-auto :noconfirm))))))

(defun adh-switch-magit-buffer ()
  "Switch to a Magit buffer."
  (interactive)
  (adh-switch-buffer-of-mode 'magit-status-mode "Magit: "))

(defun adh-magit-status-dwim ()
  "Show this repo's status without refreshing, else prompt for a repo."
  (interactive)
  (require 'magit)
  (call-interactively (if (magit-toplevel) #'magit-status-quick #'magit-status)))

(defun adh-magit-visit-file-dwim (&optional other-window)
  "Visit the worktree file from staged file headings, otherwise as usual.
With a prefix argument OTHER-WINDOW, display the buffer in another window."
  (interactive "P")
  (if (eq (magit-diff-type) 'staged)
      (magit-diff-visit-worktree-file other-window)
    (magit-diff-visit-file other-window)))

(defun adh-magit-visit-thing-other-window ()
  "Visit whatever RET would visit at point, in another window."
  (interactive)
  (let ((cmd (key-binding (kbd "RET"))))
    (cond ((memq cmd '(adh-magit-visit-file-dwim magit-diff-visit-file))
           (funcall cmd t))
          (t
           (let ((display-buffer-overriding-action '(nil (inhibit-same-window . t))))
             (call-interactively (or cmd #'magit-visit-thing)))))))

(defun adh-magit-preview-thing ()
  "Visit the thing at point in another window, keeping point here."
  (interactive)
  (save-selected-window
    (adh-magit-visit-thing-other-window)))

(defun adh-magit-blame-copy-short-hash ()
  "Copy the 7-character short hash of the blamed commit to the kill ring."
  (interactive)
  (kill-new (message "%s" (substring (oref (magit-current-blame-chunk) orig-rev) 0 7))))

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
  (let ((line (line-number-at-pos nil t))
        (magit-log-buffer-file-locked nil))
    (apply #'magit-log-buffer-file nil
           (or (magit-file-region-line-numbers) (list line line)))))

(defun adh-magit-submodule-update-all ()
  "Run git submodule update --init --recursive with the menu's arguments."
  (interactive)
  (require 'magit)
  (magit-with-toplevel
    (magit-run-git-async "submodule" "update" "--init" "--recursive"
                         (magit-submodule-arguments "--force" "--remote" "--no-fetch"
                                                    "--checkout" "--rebase" "--merge"))))

(defun adh-magit-submodule-foreach ()
  "Read the rest of a git submodule foreach command and run it at the top level."
  (interactive)
  (require 'magit)
  (magit-with-toplevel
    (let ((prefix (concat "git submodule foreach "
                          (and (magit-submodule-arguments "--recursive") "--recursive "))))
      (magit-shell-command-topdir
       (concat prefix (read-shell-command prefix nil 'adh--magit-submodule-foreach-history))))))

(use-package magit
  :ensure t :defer 10
  :init
  (setq magit-auto-revert-mode nil)
  :custom
  (magit-refresh-verbose t)
  (magit-commit-show-diff nil)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-revision-filter-files-on-follow t)
  (magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18))
  (magit-section-initial-visibility-alist
   '((staged . hide) (unstaged . hide) (untracked . hide) (stashes . hide) (unpushed . hide) (unpulled . hide)))
  :config
  (put 'magit-log-mode 'magit-log-default-arguments '("-n1024"))
  (keymap-set magit-file-section-map "<remap> <magit-visit-thing>" #'adh-magit-visit-file-dwim)
  (advice-add #'magit-show-commit :around #'adh--magit-show-commit-current-file)
  :hook
  (magit-mode . (lambda () (let ((bn (buffer-name)))
                             (when (string-match "^magit\\(.*\\): \\(.*\\)" bn)
                               (let ((kind (string-remove-prefix "-" (match-string 1 bn)))
                                     (what (match-string 2 bn)))
                                 (rename-buffer (if (string-empty-p kind)
                                                    what
                                                  (format "%s: %s" kind what))
                                                t)))))))

(with-eval-after-load 'magit-files
  (transient-replace-suffix 'magit-file-dispatch 'magit-log-trace-definition
    '("t" "Trace" adh-magit-log-trace-region-or-line))
  (transient-replace-suffix 'magit-file-dispatch 'magit-log-buffer-file
    '("l" "Log" adh-magit-log-buffer-file-follow :if-not-derived dired-mode))
  (transient-replace-suffix 'magit-file-dispatch 'magit-blame-addition
    '("b" "Blame" adh-toggle-magit-blame))
  (transient-append-suffix 'magit-file-dispatch ", c"
    '(", R" "Restore" adh-magit-restore-current)))

(with-eval-after-load 'magit-submodule
  (transient-append-suffix 'magit-submodule 'magit-fetch-modules
    '("U" "Update all modules" adh-magit-submodule-update-all))
  (transient-append-suffix 'magit-submodule 'adh-magit-submodule-update-all
    '("!" "Run in each module" adh-magit-submodule-foreach)))

(provide 'adh-magit)
