;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh--rename-mode (mode name)
  (when-let ((entry (assq mode minor-mode-alist)))
    (setcdr entry (list name))))

(defun adh--move-lines (n)
  (let* (text-start
         text-end
         (region-start (point))
         (region-end region-start)
         swap-point-mark
         delete-latest-newline)
    (when (region-active-p)
      (if (> (point) (mark))
          (setq region-start (mark))
        (exchange-point-and-mark)
        (setq swap-point-mark t
              region-end (point))))
    (end-of-line)
    (if (< (point) (point-max))
        (forward-char 1)
      (setq delete-latest-newline t)
      (insert-char ?\n))
    (setq text-end (point)
          region-end (- region-end text-end))
    (goto-char region-start)
    (beginning-of-line)
    (setq text-start (point)
          region-start (- region-start text-end))
    (let ((text (delete-and-extract-region text-start text-end)))
      (forward-line n)
      (when (not (= (current-column) 0))
        (insert-char ?\n)
        (setq delete-latest-newline t))
      (insert text))
    (forward-char region-end)
    (when delete-latest-newline
      (save-excursion
        (goto-char (point-max))
        (delete-char -1)))
    (when (region-active-p)
      (setq deactivate-mark nil)
      (set-mark (+ (point) (- region-start region-end)))
      (if swap-point-mark
          (exchange-point-and-mark)))))

(defun adh--insert-pair-around-region (open close)
  (when (use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (save-excursion
        (goto-char end)
        (insert close)
        (goto-char beg)
        (insert open)))))

(defun adh--with-saved-window (fn)
  (let ((win (selected-window)))
    (call-interactively fn)
    (select-window win)))

(defun adh--half-window-height ()
  (max 1 (/ (window-body-height) 2)))

(defun adh--tmux-command ()
  (if (eq system-type 'windows-nt)
      "wsl tmux"
    "tmux"))

(defun adh--buffer-file-name ()
  (or (buffer-file-name)
      (when (and (derived-mode-p 'dired-mode) (fboundp 'dired-get-filename))
        (ignore-errors (dired-get-filename nil t)))))

(defun adh--maybe-truename (path resolve)
  (if resolve (file-truename path) path))

(defun adh--sidewin-target-p (buf _action)
  (with-current-buffer buf
    (and (or (derived-mode-p
              'special-mode
              'comint-mode
              'compilation-mode
              'messages-buffer-mode))
         (not (derived-mode-p
               'magit-mode)))))

;;;; Interactive
(defun adh-scroll-up-half ()
  (interactive)
  (forward-line (- (adh--half-window-height)))
  (recenter))

(defun adh-scroll-down-half ()
  (interactive)
  (forward-line (adh--half-window-height))
  (recenter))

(defun adh-move-lines-up (&optional n)
  (interactive "p")
  (adh--move-lines (- (or n 1))))

(defun adh-move-lines-down (&optional n)
  (interactive "p")
  (adh--move-lines (or n 1)))

(defun adh-mark-line (&optional n)
  (interactive "p")
  (let* ((n (or n 1))
         (steps (max 1 (abs n))))
    (if (not (region-active-p))
        (progn
          (beginning-of-line)
          (set-mark (line-end-position))
          (activate-mark))
      (let* ((p (point))
             (m (mark t))
             (forward (if (> m p) t (if (< m p) nil t)))
             target)
        (save-excursion
          (goto-char m)
          (if forward
              (progn
                (forward-line steps)
                (setq target (line-end-position)))
            (forward-line (- steps))
            (setq target (line-beginning-position))))
        (set-mark target)
        (activate-mark)))))

(defun adh-duplicate-dwim ()
  (interactive)
  (unless mark-ring
    (push-mark (point) t nil))
  (let* ((col (current-column))
         (beg (if (region-active-p)
                  (region-beginning)
                (line-beginning-position)))
         (end (if (region-active-p)
                  (region-end)
                (line-end-position)))
         (lines (max 1 (count-lines beg end))))
    (save-excursion
      (goto-char end)
      (newline)
      (insert (buffer-substring-no-properties beg end)))
    (forward-line lines)
    (move-to-column col)
    (setq this-command 'yank)))

(defun adh-mark-inside ()
  (interactive)
  (mark-sexp)
  (forward-char)
  (exchange-point-and-mark)
  (backward-char)
  (exchange-point-and-mark))

(defun adh-select-side-window ()
  (interactive)
  (let ((side-windows
         (seq-filter
          (lambda (window)
            (window-parameter window 'window-side))
          (window-list))))
    (when side-windows
      (select-window (car side-windows)))))

(defun adh-to-side-window ()
  (interactive)
  (unless (window-parameter nil 'window-side)
    (let ((buf (current-buffer)))
      (delete-window)
      (display-buffer-in-side-window
       buf
       '((side . bottom)
         (window-height . 30)
         (window-parameters . ((no-other-window . t)))
         (body-function . select-window)))))
  (shrink-window-if-larger-than-buffer))

(defun adh-insert-line-above ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-for-tab-command))

(defun adh-insert-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun adh-join-line-above ()
  (interactive)
  (join-line -1))

(defun adh-delete-other-windows ()
  (interactive)
  (let ((buf (current-buffer)))
    (if (window-parameter nil 'window-side)
        (progn
          (window-toggle-side-windows)
          (delete-other-windows-internal)
          (switch-to-buffer buf))
      (delete-other-windows-internal))))

(defun adh-backward-delete-char-dwim ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-char -1)))

(defun adh-kill-line-above ()
  (interactive)
  (forward-line -1)
  (kill-whole-line))

(defun adh-kill-region-or-line ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-line)))

(defun adh-kill-other-buffers ()
  (interactive)
  (let ((current (current-buffer)))
    (dolist (buf (buffer-list))
      (unless (or (eq buf current)
                  (string= (buffer-name buf) "*scratch*"))
        (kill-buffer buf)))))

(defun adh-kill-matching-buffers-no-ask-except-current (regexp &optional internal-too)
  (interactive
   (list (read-regexp "Kill buffers: ")
         current-prefix-arg))
  (dolist (buf (buffer-list))
    (let ((name (buffer-name buf)))
      (when (and (not (eq buf (current-buffer)))
                 (or internal-too
                     (not (string-prefix-p " " name)))
                 (string-match-p regexp name))
        (kill-buffer buf)))))

(defun adh-compile-region (start end)
  (interactive "r")
  (let ((command (buffer-substring-no-properties start end)))
    (compile command)))

(defun adh-tmux-to-emacs-buffer ()
  (interactive)
  (let ((content (shell-command-to-string (format "%s capture-pane -p" (adh--tmux-command)))))
    (with-current-buffer (get-buffer-create "*tmux*")
      (erase-buffer)
      (insert content))
    (switch-to-buffer "*tmux*")
    (backward-word)
    (forward-word)))

(defun adh-tmux-to-emacs-buffer-all ()
  (interactive)
  (let ((content (shell-command-to-string (format "%s capture-pane -p -S -" (adh--tmux-command)))))
    (with-current-buffer (get-buffer-create "*tmux-all*")
      (erase-buffer)
      (insert content))
    (switch-to-buffer "*tmux-all*")
    (backward-word)
    (forward-word)))

(defun adh-tmux-cd ()
  (interactive)
  (let* ((dir-path (or (and (buffer-file-name)
                            (file-name-directory (buffer-file-name)))
                       default-directory))
         (is-windows (eq system-type 'windows-nt))
         (abs-path (if is-windows
                       dir-path
                     (expand-file-name dir-path)))
         (unix-path (if (and is-windows
                             (string-match "^\\([a-zA-Z]\\):" abs-path))
                        (concat "/" (downcase (match-string 1 abs-path))
                                (substring abs-path 2))
                      abs-path)))
    (shell-command (format "%s send-keys 'cd %s' C-m"
                           (adh--tmux-command)
                           (shell-quote-argument (directory-file-name unix-path))))
    (message "Changed tmux directory to '%s'" unix-path)))

(defun adh-dired-or-file ()
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (call-interactively 'find-file)
    (dired-jump)))

(defun adh-dired-duplicate-file ()
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (dir  (file-name-directory file))
         (name (file-name-nondirectory file))
         (base (file-name-sans-extension name))
         (ext  (file-name-extension name t))
         new)
    (if (string-match "\\(.*?\\)_copy\\([0-9]*\\)$" base)
        (setq base (match-string 1 base)))
    (setq new (expand-file-name (concat base "_copy" ext) dir))
    (let ((i 1))
      (while (file-exists-p new)
        (setq new (expand-file-name (format "%s_copy%d%s" base i ext) dir))
        (setq i (1+ i))))
    (copy-file file new)
    (dired-add-file new)
    (message "Copied %s -> %s" name (file-name-nondirectory new))))

;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-emacs.org
(defun adh-keyboard-quit-dwim ()
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(defun adh-sort-u ()
  (interactive)
  (let ((beg (if (region-active-p)
                 (region-beginning)
               (point-min)))
        (end (if (region-active-p)
                 (region-end)
               (point-max))))
    (sort-lines nil beg end)
    (delete-duplicate-lines beg end)))

(defun adh-wrap-region-with-pair ()
  (interactive)
  (when (use-region-p)
    (let ((char (read-char "Wrap with: ")))
      (cond
       ((eq char ?\() (adh--insert-pair-around-region "(" ")"))
       ((eq char ?\[) (adh--insert-pair-around-region "[" "]"))
       ((eq char ?\{) (adh--insert-pair-around-region "{" "}"))
       ((eq char ?<) (adh--insert-pair-around-region "<" ">"))
       (t (let ((char-str (char-to-string char)))
            (adh--insert-pair-around-region char-str char-str)))))))

(defun adh-down-list (&optional n)
  (interactive "p")
  (let ((count (or n 1)))
    (dotimes (_ count)
      (let (done)
        (while (not done)
          (let ((pt (point)))
            (or
             (ignore-errors
               (down-list 1)
               (setq done t))
             (progn
               (ignore-errors
                 (backward-up-list 1)
                 (forward-sexp 1)
                 (skip-syntax-forward " >"))
               (when (= (point) pt)
                 (setq done t))))))))))

(defun adh-copy-file-name ()
  (interactive)
  (if-let* ((f (adh--buffer-file-name)))
      (let ((name (file-name-nondirectory f)))
        (kill-new name)
        (message "Copied %s" name)
        name)
    (let ((name (buffer-name)))
      (kill-new name)
      (message "Copied buffer name %s" name)
      name)))

(defun adh-copy-path (&optional resolve)
  (interactive "P")
  (let* ((file (or (adh--buffer-file-name) default-directory))
         (dir  (file-name-directory (expand-file-name file)))
         (path (adh--maybe-truename (directory-file-name dir) resolve)))
    (kill-new path)
    (message "Copied %s" path)
    path))

(defun adh-copy-full-path (&optional resolve)
  (interactive "P")
  (let* ((file (or (adh--buffer-file-name) default-directory))
         (path (adh--maybe-truename (expand-file-name file) resolve)))
    (kill-new path)
    (message "Copied %s" path)
    path))

(defun adh-completion-in-region-isearch()
  (interactive)
  (select-window (get-buffer-window "*Completions*" t))
  (isearch-forward))

(defun adh-show-buffer-file-encoding ()
  (interactive)
  (message "Buffer encoding: %s" buffer-file-coding-system))

(defun adh-minibuffer-next-history-or-clear (n)
  (interactive "p")
  (condition-case nil
      (next-history-element n)
    (error (delete-minibuffer-contents))))

(defun adh-compile-goto-error-and-pop ()
  (interactive)
  (adh--with-saved-window #'compile-goto-error))

(defun adh-occur-goto-and-pop ()
  (interactive)
  (adh--with-saved-window #'occur-mode-goto-occurrence))

(defun adh-time-command (command)
  (interactive "CCommand to time: ")
  (let ((time (benchmark-run 1 (call-interactively command))))
    (message "[adh] %s took %.3fs (GCs: %d)"
             command (nth 0 time) (nth 1 time))))

(provide 'adh-functions)
