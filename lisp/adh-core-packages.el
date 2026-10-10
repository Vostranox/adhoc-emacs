;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-functions)

(declare-function glasses-mode--set-explicitly "glasses" t t)

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

(defun adh-isearch-occur ()
  "Run `occur' on the current isearch and exit isearch."
  (interactive)
  (call-interactively #'isearch-occur)
  (isearch-done))

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (isearch-case-fold-search t)
  (search-whitespace-regexp ".*?")
  (lazy-count-prefix-format "(%s/%s) ")
  :config
  (advice-add 'isearch-occur :after #'adh--rename-isearch-occur-buffer))

(defvar-local adh--occur-edit-changes nil
  "Source buffers, modification ticks, and original lines for Occur editing.")

(defun adh--occur-source-buffers ()
  "Return the buffers the lines of the current Occur buffer come from."
  (save-restriction
    (widen)
    (let ((pos (point-min)) bufs)
      (while pos
        (when-let* ((target (get-text-property pos 'occur-target))
                    (buf (marker-buffer (if (consp target) (caar target) target))))
          (setq buf (or (buffer-base-buffer buf) buf))
          (unless (memq buf bufs)
            (push buf bufs)))
        (setq pos (next-single-property-change pos 'occur-target)))
      bufs)))

(defun adh--occur-edit-plain-input (beg end _old-len)
  "Strip faces from the text inserted between BEG and END."
  (with-silent-modifications
    (remove-text-properties beg end '(face nil font-lock-face nil))))

(defun adh--occur-edit-start ()
  "Record this Occur session's source lines as they are edited."
  (setq adh--occur-edit-changes
        (mapcar (lambda (buf)
                  (cons buf (list :tick (buffer-chars-modified-tick buf)
                                  :external nil :lines nil)))
                (adh--occur-source-buffers)))
  (remove-hook 'after-change-functions #'occur-after-change-function t)
  (add-hook 'after-change-functions #'adh--occur-edit-propagate nil t)
  (add-hook 'after-change-functions #'adh--occur-edit-plain-input nil t)
  (add-hook 'change-major-mode-hook #'adh--occur-edit-end nil t)
  (add-hook 'kill-buffer-hook #'adh--occur-edit-end nil t))

(defun adh--occur-source-saved ()
  "Remember source saves so abort cannot mark unsaved restored text clean."
  (let ((source (or (buffer-base-buffer) (current-buffer))))
    (dolist (buf (buffer-list))
      (when-let* ((state (cdr (assq source (buffer-local-value 'adh--occur-edit-changes buf)))))
        (plist-put state :saved t)))))

(defun adh--occur-edit-propagate (beg end old-length)
  "Propagate an Occur edit and retain the original source line.
BEG, END and OLD-LENGTH are the arguments to `after-change-functions'."
  (let* ((target (get-text-property
                  (save-excursion (goto-char beg) (line-beginning-position))
                  'occur-target))
         (marker (if (consp target) (caar target) target))
         (source (and (markerp marker) (marker-buffer marker)))
         (source (and source (or (buffer-base-buffer source) source)))
         (state (cdr (assq source adh--occur-edit-changes)))
         new-line old-tick)
    (when state
      (with-current-buffer source
        (setq old-tick (buffer-chars-modified-tick))
        (unless (= old-tick (plist-get state :tick))
          (plist-put state :external t))
        (save-restriction
          (widen)
          (unless (plist-get state :lines)
            (plist-put state :modified (buffer-modified-p))
            (plist-put state :saved nil)
            (plist-put state :hash (secure-hash 'sha1 (current-buffer))))
          (save-excursion
            (goto-char marker)
            (let ((start (line-beginning-position))
                  (finish (line-end-position)))
              (unless (seq-some (lambda (line) (= (car line) start))
                                (plist-get state :lines))
                (setq new-line (list (copy-marker start)
                                     (copy-marker finish t)
                                     (buffer-substring start finish)))
                (plist-put state :lines
                           (cons new-line (plist-get state :lines)))))))))
    (unwind-protect
        (occur-after-change-function beg end old-length)
      (when (and state (buffer-live-p source))
        (let ((tick (buffer-chars-modified-tick source)))
          (plist-put state :tick tick)
          (when (and new-line (= old-tick tick))
            (plist-put state :lines (delq new-line (plist-get state :lines)))
            (set-marker (car new-line) nil)
            (set-marker (cadr new-line) nil)))))))

(defun adh--occur-restore-line (line)
  "Restore a recorded source LINE without replacing unchanged text."
  (let ((source (current-buffer)))
    (with-temp-buffer
      (insert (caddr line))
      (let ((replacement (current-buffer)))
        (with-current-buffer source
          (combine-change-calls (marker-position (car line))
              (marker-position (cadr line))
            (let ((inhibit-modification-hooks t))
              (replace-region-contents (car line) (cadr line) replacement))))))))

(defun adh--occur-edit-end (&optional abort)
  "End this Occur session, restoring edited source lines when ABORT.
Refuse to abort if a source was edited outside this Occur session."
  (let ((edited (seq-filter (lambda (entry) (plist-get (cdr entry) :lines))
                            adh--occur-edit-changes)))
    (when abort
      (dolist (entry edited)
        (unless (and (buffer-live-p (car entry))
                     (not (plist-get (cdr entry) :external))
                     (= (buffer-chars-modified-tick (car entry))
                        (plist-get (cdr entry) :tick)))
          (user-error "Cannot abort Occur: source %s changed outside this session; finish editing and undo manually"
                      (buffer-name (car entry))))
        (with-current-buffer (car entry)
          (barf-if-buffer-read-only)))
      (let ((group (mapcan (lambda (entry) (prepare-change-group (car entry))) edited))
            accepted)
        (unwind-protect
            (progn
              (activate-change-group group)
              (dolist (entry edited)
                (with-current-buffer (car entry)
                  (save-restriction
                    (widen)
                    (save-excursion
                      (dolist (line (plist-get (cdr entry) :lines))
                        (adh--occur-restore-line line)))
                    (unless (equal (secure-hash 'sha1 (current-buffer))
                                   (plist-get (cdr entry) :hash))
                      (user-error "Cannot abort Occur: source %s changed outside its recorded lines"
                                  (buffer-name)))
                    (lock-buffer))))
              (accept-change-group group)
              (setq accepted t)
              (dolist (entry edited)
                (unless (or (plist-get (cdr entry) :modified)
                            (plist-get (cdr entry) :saved))
                  (with-current-buffer (car entry)
                    (set-buffer-modified-p nil)))))
          (unless accepted
            (cancel-change-group group))
          (dolist (entry edited)
            (when (buffer-live-p (car entry))
              (plist-put (cdr entry) :tick (buffer-chars-modified-tick (car entry))))))))
    (dolist (entry edited)
      (dolist (line (plist-get (cdr entry) :lines))
        (set-marker (car line) nil)
        (set-marker (cadr line) nil)))
    (setq adh--occur-edit-changes nil)))

(defun adh-occur-edit-abort ()
  "Restore this Occur session's source edits and leave editing mode.
If a source was also edited directly, keep all edits and refuse to abort."
  (interactive)
  (unless (derived-mode-p 'occur-edit-mode)
    (user-error "Not editing an Occur buffer"))
  (adh--occur-edit-end t)
  (occur-cease-edit)
  (revert-buffer nil t)
  (set-buffer-modified-p nil))

(defun adh-occur-edit-save ()
  "Leave `occur-edit-mode' and save the source files edited in it."
  (interactive)
  (unless (derived-mode-p 'occur-edit-mode)
    (user-error "Not editing an Occur buffer"))
  (dolist (entry adh--occur-edit-changes)
    (let ((buf (car entry)))
      (when (and (plist-get (cdr entry) :lines)
                 (buffer-live-p buf) (buffer-file-name buf) (buffer-modified-p buf))
        (with-current-buffer buf (save-buffer)))))
  (occur-cease-edit)
  (set-buffer-modified-p nil))

(use-package replace
  :ensure nil :defer t
  :hook
  (occur-edit-mode . adh--occur-edit-start)
  (after-save . adh--occur-source-saved))

(defun adh-switch-dired-buffer ()
  "Switch to a Dired buffer."
  (interactive)
  (adh-switch-buffer-of-mode 'dired-mode "Dired: "))

(defvar ls-lisp-use-insert-directory-program)

(defun adh-dired-sort-toggle-or-edit (&optional arg)
  "`dired-sort-toggle-or-edit' with ARG, via external ls on Windows.
ls-lisp ignores sort switches."
  (interactive "P" dired-mode)
  (if (eq system-type 'windows-nt)
      (let ((ls-lisp-use-insert-directory-program t))
        (dired-sort-toggle-or-edit arg))
    (dired-sort-toggle-or-edit arg)))

(defun adh-dired-or-file ()
  "In Dired, open a file; elsewhere, jump to the current file in Dired."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (call-interactively 'find-file)
    (dired-jump)))

(defun adh-dired-duplicate-dwim ()
  "Copy each marked file or directory to a numbered `_copy' sibling."
  (interactive)
  (let ((files (dired-get-marked-files t current-prefix-arg)))
    (dolist (file files)
      (setq file (directory-file-name file))
      (let* ((dir  (file-name-directory file))
             (name (file-name-nondirectory file))
             (base (file-name-sans-extension name))
             (ext  (or (file-name-extension name t) ""))
             (clean-base (if (string-match "\\(.*\\)_copy[0-9]*$" base)
                             (match-string 1 base)
                           base))
             (new-name (concat clean-base "_copy" ext))
             (new-path (expand-file-name new-name dir))
             (i 2))
        (while (file-exists-p new-path)
          (setq new-path (expand-file-name
                          (concat clean-base "_copy" (number-to-string i) ext)
                          dir))
          (setq i (1+ i)))
        (if (file-directory-p file)
            (copy-directory file new-path t t t)
          (copy-file file new-path nil t t t))
        (dired-add-file new-path)))
    (revert-buffer)
    (message "Duplicated %d item(s)." (length files))))

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

(defun adh--apply-which-key (on)
  "Turn `which-key-mode' ON or off; inside a transient menu, once it exits."
  (remove-hook 'transient-exit-hook 'transient--resume-which-key-mode)
  (cond ((and on (bound-and-true-p transient--prefix))
         (add-hook 'transient-exit-hook 'transient--resume-which-key-mode))
        (on (which-key-mode 1))
        ((bound-and-true-p which-key-mode) (which-key-mode -1))))

(use-package which-key
  :ensure nil :defer t
  :custom
  (which-key-popup-type 'minibuffer)
  (which-key-lighter nil))

(adh--apply-vc adh-use-vc)
(adh--apply-subwords adh-subwords)
(adh--apply-which-key adh-use-which-key)

(provide 'adh-core-packages)

;;; adh-core-packages.el ends here
