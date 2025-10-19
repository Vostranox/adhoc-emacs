;;; -*- lexical-binding: t; coding: utf-8 -*-

(defvar adh--font-hook nil
  "Frame hook installed by `adh-set-font' to apply the font to new frames.")
(defconst adh--tmux-command (if (eq system-type 'windows-nt) "wsl tmux" "tmux")
  "Shell command used to talk to tmux (via WSL on Windows).")
(defconst adh--minibuffer-pivot-delay 0.000001
  "Idle seconds before `adh--minibuffer-pivot' reopens the minibuffer.")

(defun adh--minibuffer-pivot (command)
  "Abort the current minibuffer and reopen it under COMMAND, keeping the input."
  (let ((input (minibuffer-contents)))
    (run-with-idle-timer adh--minibuffer-pivot-delay nil
     (lambda ()
       (minibuffer-with-setup-hook
           (lambda ()
             (setq this-command command)
             (insert input))
         (call-interactively command))))
    (abort-recursive-edit)))

(defun adh--rename-mode (mode name)
  "Set MODE's mode-line lighter to NAME."
  (when-let* ((entry (assq mode minor-mode-alist)))
    (setcdr entry (list name))))

(defun adh--with-saved-window (fn)
  "Call FN interactively without letting it change the selected window."
  (save-selected-window
    (call-interactively fn)))

(defun adh--half-window-height ()
  "Return half the current window's body height, at least 1."
  (max 1 (/ (window-body-height) 2)))

(defun adh--buffer-file-name ()
  "Return the current buffer's file, or the file at point in Dired."
  (or (buffer-file-name)
      (when (and (derived-mode-p 'dired-mode) (fboundp 'dired-get-filename))
        (ignore-errors (dired-get-filename nil t)))))

(defun adh--maybe-truename (path resolve)
  "Return PATH, resolved through symlinks when RESOLVE is non-nil."
  (if resolve (file-truename path) path))

(defun adh--move-lines (n)
  "Move the current line or region N lines down."
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

(defun adh--sidewin-target-p (buf _action)
  "Return non-nil if BUF is an output buffer that belongs in a side window."
  (with-current-buffer buf
    (and (derived-mode-p
          'special-mode
          'comint-mode
          'compilation-mode
          'messages-buffer-mode)
         (not (derived-mode-p 'magit-mode)))))

(defun adh--snake-to-pascal (str)
  "Convert snake_case STR to PascalCase."
  (mapconcat #'capitalize
             (split-string str "_" t)
             ""))

(defun adh--camel-to-snake (str)
  "Convert camelCase or PascalCase STR to snake_case."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string
      "\\([a-z0-9]\\)\\([A-Z]\\)"
      "\\1_\\2"
      str))))

(defun adh--toggle-case-string (str)
  "Toggle STR between snake_case and PascalCase."
  (if (string-match-p "_" str)
      (adh--snake-to-pascal str)
    (adh--camel-to-snake str)))

(defun adh--apply-font (family height &optional frame)
  "Set the default face to FAMILY at HEIGHT in FRAME.
Return non-nil on success, nil if non-graphical or the font is missing."
  (with-selected-frame (or frame (selected-frame))
    (if (and (display-graphic-p)
             (find-font (font-spec :family family)))
        (progn
          (set-face-attribute 'default nil :family family :height height)
          t)
      nil)))

(defun adh--apply-frame-parameter (parameter value)
  "Set frame PARAMETER to VALUE for the current and all future frames."
  (set-frame-parameter nil parameter value)
  (setf (alist-get parameter default-frame-alist) value))

(defun adh-scroll-up-half ()
  "Move point up half a window and recenter."
  (interactive)
  (forward-line (- (adh--half-window-height)))
  (recenter))

(defun adh-scroll-down-half ()
  "Move point down half a window and recenter."
  (interactive)
  (forward-line (adh--half-window-height))
  (recenter))

(defun adh-move-lines-up (&optional n)
  "Move the current line or region up N lines."
  (interactive "p")
  (adh--move-lines (- (or n 1))))

(defun adh-move-lines-down (&optional n)
  "Move the current line or region down N lines."
  (interactive "p")
  (adh--move-lines (or n 1)))

(defun adh-mark-line (&optional n)
  "Mark the current line, or extend an active region by N whole lines."
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

(defun adh-duplicate-dwim (&optional n)
  "Duplicate the current line, or the region extended to whole lines, N times."
  (interactive "p")
  (when (use-region-p)
    (let ((beg (save-excursion (goto-char (region-beginning))
                               (line-beginning-position))))
      (goto-char (region-end))
      (unless (bolp)
        (if (eobp)
            (insert "\n")
          (forward-line 1)))
      (set-mark beg)))
  (duplicate-dwim n))

(defun adh-mark-inside ()
  "Mark the contents of the sexp at point, excluding its delimiters."
  (interactive)
  (mark-sexp)
  (forward-char)
  (exchange-point-and-mark)
  (backward-char)
  (exchange-point-and-mark))

(defun adh-insert-line-above ()
  "Open and indent a new line above the current one."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-for-tab-command))

(defun adh-insert-line-below ()
  "Open and indent a new line below the current one."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun adh-join-line-above ()
  "Join the current line to the one above."
  (interactive)
  (join-line -1))

(defun adh-backward-delete-char-dwim ()
  "Delete the active region, or the previous character when there is none."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-char -1)))

(defun adh-kill-line-above ()
  "Kill the whole line above point."
  (interactive)
  (forward-line -1)
  (kill-whole-line))

(defun adh-kill-region-or-line ()
  "Kill the active region, or to end of line when there is none."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-line)))

(defun adh-sort-u ()
  "Sort the region (or whole buffer) and remove duplicate lines."
  (interactive)
  (let ((beg (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (sort-lines nil beg end)
    (delete-duplicate-lines beg end)))

(defun adh-wrap-region-with-pair ()
  "Wrap the region with a pair read from the keyboard."
  (interactive)
  (when (use-region-p)
    (let* ((char (read-char "Wrap with: "))
           (close (or (cadr (assq char insert-pair-alist)) char)))
      (insert-pair 1 char close))))

(defun adh-down-list (&optional n)
  "Move forward and down into the next list, N times.
Unlike `down-list', step over sexps that open no sublist instead of
erroring, so point lands inside the next list ahead."
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

(defun adh-toggle-func-case-at-region (start end)
  "Toggle the text between START and END between snake_case and PascalCase."
  (interactive "r")
  (atomic-change-group
    (let* ((text (delete-and-extract-region start end))
           (new-text (adh--toggle-case-string text)))
      (insert new-text))))

(defun adh-toggle-func-case-at-point ()
  "Toggle the symbol at point (or region) between snake_case and PascalCase."
  (interactive)
  (let ((bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (bounds-of-thing-at-point 'symbol))))
    (when bounds
      (adh-toggle-func-case-at-region (car bounds) (cdr bounds)))))

(defun adh-split-below-root ()
  "Split the frame's root window below."
  (interactive)
  (split-window (frame-root-window) nil 'below))

(defun adh-split-right-root ()
  "Split the frame's root window to the right."
  (interactive)
  (split-window (frame-root-window) nil 'right))

(defun adh-select-side-window ()
  "Select the first side window, if any."
  (interactive)
  (when-let* ((win (window-with-parameter 'window-side)))
    (select-window win)))

(defun adh-to-side-window ()
  "Move the current buffer into a bottom side window."
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

(defun adh-delete-other-windows ()
  "Delete all other windows, restoring the current buffer if it was a side window."
  (interactive)
  (let ((buf (current-buffer)))
    (if (window-parameter nil 'window-side)
        (progn
          (window-toggle-side-windows)
          (delete-other-windows-internal)
          (switch-to-buffer buf))
      (delete-other-windows-internal))))

(defun adh-switch-to-buffer ()
  "Switch to a buffer, hiding hidden and special (* or space) buffers."
  (interactive)
  (switch-to-buffer
   (read-buffer "Buffer: " nil t
                #'(lambda (arg)
                    (not (string-match-p "^[ *]" (car arg)))))))

(defun adh-kill-other-buffers ()
  "Kill all buffers except the current one and *scratch*."
  (interactive)
  (let ((current (current-buffer)))
    (dolist (buf (buffer-list))
      (unless (or (eq buf current)
                  (string= (buffer-name buf) "*scratch*"))
        (kill-buffer buf)))))

(defun adh-kill-matching-buffers-no-ask-except-current (regexp &optional internal-too)
  "Kill all buffers whose name matches REGEXP, without confirmation."
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
            (setq count (1+ count))))))
    (message "Killed %d buffer(s)." count)))

(defun adh-copy-file-name ()
  "Copy the current buffer's file name (or buffer name) to the kill ring."
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
  "Copy the current file's directory to the kill ring.
With RESOLVE (prefix arg), follow symlinks."
  (interactive "P")
  (let* ((file (or (adh--buffer-file-name) default-directory))
         (dir  (file-name-directory (expand-file-name file)))
         (path (adh--maybe-truename (directory-file-name dir) resolve)))
    (kill-new path)
    (message "Copied %s" path)
    path))

(defun adh-copy-full-path (&optional resolve)
  "Copy the current file's full path to the kill ring.
With RESOLVE (prefix arg), follow symlinks."
  (interactive "P")
  (let* ((file (or (adh--buffer-file-name) default-directory))
         (path (adh--maybe-truename (expand-file-name file) resolve)))
    (kill-new path)
    (message "Copied %s" path)
    path))

(defun adh--dired-sort-toggle-or-edit-windows ()
  "Like `dired-sort-toggle-or-edit', but force the external ls.
Needed on Windows, where ls-lisp otherwise ignores the sort switches."
  (interactive)
  (let ((ls-lisp-use-insert-directory-program t))
    (dired-sort-toggle-or-edit)))

(defun adh-dired-or-file ()
  "In Dired, open a file; elsewhere, jump to the current file in Dired."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (call-interactively 'find-file)
    (dired-jump)))

(defun adh-dired-duplicate-dwim ()
  "Copy each marked Dired file or directory to a `_copy' sibling.
A numeric suffix is added as needed to avoid overwriting."
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

;; Use the external-ls variant on Windows, the builtin command elsewhere.
(defalias 'adh-dired-sort-toggle-or-edit
  (if (eq system-type 'windows-nt)
      #'adh--dired-sort-toggle-or-edit-windows
    #'dired-sort-toggle-or-edit))

(defun adh-set-window-decoration (decorated)
  "Show or hide the window-manager frame decorations per DECORATED."
  (setq adh-window-decoration decorated)
  (adh--apply-frame-parameter 'undecorated (not decorated)))

(defun adh-set-frame-opacity (opacity)
  "Set frame OPACITY (0-100) for the current and future frames."
  (setq adh-frame-opacity opacity)
  (adh--apply-frame-parameter 'alpha adh-frame-opacity))

(defun adh-set-font (family height)
  "Set the default font to FAMILY at HEIGHT, including for future frames."
  (when adh--font-hook (remove-hook 'after-make-frame-functions adh--font-hook))
  (setq adh--font-hook
        (lambda (frame)
          (unless (adh--apply-font family height frame)
            (when (display-graphic-p frame)
              (message "[adh] Warning: Queued font '%s' not found." family)))))
  (add-hook 'after-make-frame-functions adh--font-hook)
  (if (adh--apply-font family height (selected-frame))
      (message "[adh] Set font '%s'" family)
    (if (display-graphic-p)
        (message "[adh] Font not found: '%s'" family)
      (message "[adh] Queued font '%s'" family))))

(defun adh-toggle-window-decorations ()
  "Toggle the window-manager frame decorations."
  (interactive)
  (adh-set-window-decoration (not adh-window-decoration)))

(defun adh-set-file-extension-mode (ext mode)
  "Open files with extension EXT in MODE."
  (add-to-list 'auto-mode-alist (cons (format "\\.%s\\'" ext) mode)))

(defun adh-add-to-path (path)
  "Prepend PATH to both `exec-path' and the PATH environment variable."
  (let ((expanded-path (expand-file-name path)))
    (add-to-list 'exec-path expanded-path)
    (let ((current-path (getenv "PATH")))
      (unless (member expanded-path (split-string current-path path-separator))
        (setenv "PATH" (concat expanded-path path-separator current-path))))))

(defun adh-get-executable ()
  "Get an executable on PATH and jump to it in Dired."
  (interactive)
  (let* ((exec-path (if (eq system-type 'windows-nt) (remove "." exec-path) exec-path))
         (completion-extra-properties
          `(:group-function
            ,(lambda (cand transform)
               (if transform cand
                 (if-let (path (locate-file cand exec-path '() 'file-executable-p))
                     (file-name-directory path) "non-executable")))))
         (exe (completing-read "Exe: " (apply-partially #'locate-file-completion-table exec-path '()) nil t)))
    (dired-jump nil (locate-file exe exec-path '()))))

(defun adh-getenv ()
  "Show an environment variable and copy its value to the kill ring."
  (interactive)
  (let ((val (call-interactively #'getenv)))
    (when val (kill-new val))))

(defalias 'adh-setenv #'setenv)

(defun adh-tmux-to-emacs-buffer ()
  "Capture the visible tmux pane into the *tmux* buffer."
  (interactive)
  (let ((content (shell-command-to-string (format "%s capture-pane -p" adh--tmux-command))))
    (with-current-buffer (get-buffer-create "*tmux*")
      (erase-buffer)
      (insert content))
    (switch-to-buffer "*tmux*")
    (goto-char (point-max))
    (skip-chars-backward " \t\n")))

(defun adh-tmux-to-emacs-buffer-all ()
  "Capture the full tmux pane history into the *tmux-all* buffer."
  (interactive)
  (let ((content (shell-command-to-string (format "%s capture-pane -p -S -" adh--tmux-command))))
    (with-current-buffer (get-buffer-create "*tmux-all*")
      (erase-buffer)
      (insert content))
    (switch-to-buffer "*tmux-all*")
    (goto-char (point-max))
    (skip-chars-backward " \t\n")))

(defun adh-tmux-cd (&optional target)
  "Send a `cd' to Emacs's current directory to tmux, asynchronously."
  (interactive)
  (let* ((buf (window-buffer (selected-window)))
         (dir-path (with-current-buffer buf
                     (or (and buffer-file-name
                              (file-name-directory buffer-file-name))
                         default-directory)))
         (is-windows (eq system-type 'windows-nt))
         (abs-path (if is-windows
                       dir-path
                     (expand-file-name dir-path)))
         (unix-path (if (and is-windows
                             (string-match "^\\([a-zA-Z]\\):" abs-path))
                        (concat "/" (downcase (match-string 1 abs-path))
                                (substring abs-path 2))
                      abs-path))
         (dest (or target adh-tmux-cd-session))
         (tflag (if dest (concat " -t " (shell-quote-argument dest)) "")))
    (start-process-shell-command
     "adh-tmux-cd" nil
     (format "%s send-keys%s 'cd %s' C-m"
             adh--tmux-command tflag
             (shell-quote-argument (directory-file-name unix-path))))
    (when (called-interactively-p 'interactive)
      (message "tmux directory -> '%s'" unix-path))
    unix-path))

(defun adh-kill-process (&optional sigkill)
  "Pick a system process and signal it: SIGTERM, or SIGKILL with a prefix arg."
  (interactive "P")
  (let* ((kill (or sigkill (eq system-type 'windows-nt)))
         (cands
          (delq nil
                (mapcar
                 (lambda (pid)
                   (let* ((a    (process-attributes pid))
                          (args (alist-get 'args a))
                          (comm (alist-get 'comm a))
                          (exe  (ignore-errors
                                  (file-symlink-p (format "/proc/%d/exe" pid))))
                          (desc (or args comm)))
                     (and desc
                          (cons (format "%-7d %-10s %-40s %s"
                                        pid
                                        (or (alist-get 'user a) "?")
                                        (or exe comm "?")
                                        desc)
                                pid))))
                 (list-system-processes))))
         (choice (completing-read
                  (format "Kill [%s]: " (if kill "KILL" "TERM"))
                  cands nil t))
         (pid (cdr (assoc choice cands))))
    (when pid
      (let ((sig (if kill 'SIGKILL 'SIGTERM)))
        (signal-process pid sig)
        (message "Sent %s to %d" sig pid)))))

(defun adh-compile-region (start end)
  "Run the region between START and END as a `compile' command."
  (interactive "r")
  (let ((command (buffer-substring-no-properties start end)))
    (compile command)))

(defun adh-completion-in-region-isearch()
  "Jump to the *Completions* window and start isearch."
  (interactive)
  (select-window (get-buffer-window "*Completions*" t))
  (isearch-forward))

(defun adh-isearch-occur ()
  "Run `occur' on the current isearch and exit isearch."
  (interactive)
  (call-interactively #'isearch-occur)
  (isearch-done))

(defun adh-minibuffer-next-history-or-clear (n)
  "Insert the next history element, or clear the minibuffer at end of history."
  (interactive "p")
  (condition-case nil
      (next-history-element n)
    (error (delete-minibuffer-contents))))

(defun adh-apropos ()
  "Run `apropos' on the region or symbol at point, prompting if there is none."
  (interactive)
  (let ((search-term (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (thing-at-point 'symbol t))))
    (if search-term
        (apropos search-term)
      (call-interactively 'apropos))))

(defun adh-show-buffer-file-encoding ()
  "Show the current buffer's file coding system."
  (interactive)
  (message "Buffer encoding: %s" buffer-file-coding-system))

(defun adh-keyboard-quit-dwim ()
  "Quit the minibuffer if one is active, otherwise run `keyboard-quit'."
  (interactive)
  (if (> (minibuffer-depth) 0)
      (abort-recursive-edit)
    (keyboard-quit)))

(defun adh-subword-toggle ()
  "Toggle camelCase-aware word motion and display.
Turns `subword-mode' (so word commands stop at camelCase boundaries) and
`glasses-mode' (which visually separates those humps) on or off together."
  (interactive)
  (glasses-mode 'toggle)
  (subword-mode 'toggle))

(defun adh-toggle-ide-mode ()
  "Toggle the IDE stack: completion, eglot, flymake and format-on-save."
  (interactive)
  (adh-toggle-cmp-auto)
  (adh-toggle-eglot-flymake)
  (adh-toggle-eglot-format-on-save)
  (adh-toggle-eglot-global))

(provide 'adh-functions)
