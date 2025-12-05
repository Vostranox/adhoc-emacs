;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh--rename-mode (mode name)
  (when-let ((entry (assq mode minor-mode-alist)))
    (setcdr entry (list name))))

(defun adh--move-lines (n)
  (let* ((use-region (use-region-p))
         (beg (if use-region (region-beginning) (point)))
         (end (if use-region (region-end) (point)))
         (line-start (save-excursion (goto-char beg) (line-beginning-position)))
         (line-end (save-excursion
                     (goto-char end)
                     (if (and use-region (bolp) (> end beg))
                         (point)
                       (line-beginning-position 2))))
         (point-offset (- (point) line-start))
         (mark-offset (when use-region (- (mark) line-start)))
         (raw-text (delete-and-extract-region line-start line-end))
         (text (if (string-suffix-p "\n" raw-text)
                   raw-text
                 (concat raw-text "\n"))))
    (forward-line n)
    (when (and (eobp) (not (bolp)))
      (insert "\n"))
    (let ((new-start (point)))
      (insert text)
      (if use-region
          (progn
            (set-mark (+ new-start mark-offset))
            (goto-char (+ new-start point-offset))
            (setq deactivate-mark nil))
        (goto-char (+ new-start point-offset))))))

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
(defun adh-split-below-root ()
  (interactive)
  (split-window (frame-root-window) nil 'below))

(defun adh-split-right-root ()
  (interactive)
  (split-window (frame-root-window) nil 'right))

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
  (let ((steps (max 1 (abs (or n 1)))))
    (if (not (use-region-p))
        (progn
          (beginning-of-line)
          (set-mark
           (if (eolp)
               (line-beginning-position 2)
             (line-end-position)))
          (activate-mark))
      (save-excursion
        (let ((mark-is-below (>= (mark) (point))))
          (goto-char (mark))
          (if mark-is-below
              (progn
                (forward-line steps)
                (end-of-line))
            (forward-line (- steps))
            (beginning-of-line))
          (set-mark (point)))))))

(defun adh-duplicate-dwim ()
  (interactive)
  (let* ((use-region (use-region-p))
         (beg (if use-region (region-beginning) (line-beginning-position)))
         (end (if use-region (region-end) (line-end-position)))
         (text (buffer-substring beg end))
         (col (current-column)))
    (save-excursion
      (goto-char end)
      (newline)
      (insert text))
    (forward-line (if use-region (count-lines beg end) 1))
    (move-to-column col)
    (when use-region
      (set-mark (+ (mark) (length text) 1))
      (setq deactivate-mark nil))))

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
   (list (read-regexp "Kill buffers (regexp): ")
         current-prefix-arg))
  (let ((count 0))
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        (when (and (not (eq buf (current-buffer)))
                   (or internal-too
                       (not (string-prefix-p " " name)))
                   (string-match-p regexp name))
          (when (kill-buffer buf)
            (setq count (1+ count))))))))

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

(defun adh-dired-duplicate-dwim ()
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
             (i 1))
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

(defun adh-keyboard-quit-dwim ()
  (interactive)
  (if (> (minibuffer-depth) 0)
      (abort-recursive-edit)
    (keyboard-quit)))

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

(provide 'adh-functions)
