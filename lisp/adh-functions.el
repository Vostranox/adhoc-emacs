;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-vars)

(defvar vertico-count)

(defvar ls-lisp-use-insert-directory-program)

(defvar adh--font-hook nil
  "Frame hook installed by `adh--apply-font-settings' for new frames.")
(defconst adh--tmux-command (if (eq system-type 'windows-nt) "wsl tmux" "tmux")
  "Shell command used to talk to tmux (via WSL on Windows).")
(defconst adh--minibuffer-pivot-delay 0.000001
  "Idle seconds before `adh--minibuffer-pivot' reopens the minibuffer.")

(defun adh--minibuffer-pivot (command)
  "Abort the minibuffer and reopen it under COMMAND with the same input."
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

(defun adh--popup-buffer-p (buf)
  "Return non-nil if BUF matches an entry of `adh-popup-buffers'."
  (with-current-buffer buf
    (seq-some (lambda (entry)
                (if (stringp entry)
                    (string-match-p entry (buffer-name))
                  (derived-mode-p entry)))
              adh-popup-buffers)))

(defun adh--apply-font (family height &optional frame)
  "Set the default face to FAMILY at HEIGHT in FRAME.
Return nil on a terminal or if the font is missing."
  (with-selected-frame (or frame (selected-frame))
    (if (and (display-graphic-p)
             (find-font (font-spec :family family)))
        (progn
          (set-face-attribute 'default nil :family family :height height)
          (set-face-attribute 'fixed-pitch nil :family family)
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
  "Duplicate the line, or the region's whole lines, N times."
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
  "Move into the next list N times, stepping over atoms `down-list' errors on."
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

(defun adh--popper-window-height (win)
  "Set popup window WIN's height, capped at `adh-list-max-height'.
Output buffers (compile, grep, shell) get the cap up front; others fit."
  (let* ((buf (window-buffer win))
         (max adh-list-max-height)
         (room (+ (window-total-height win) (window-max-delta win)))
         (growing (or (get-buffer-process buf)
                      (local-variable-p 'compilation-directory buf))))
    (fit-window-to-buffer win max (and growing (min max room)))))

(defun adh--popper-display (buffer &optional alist)
  "Show popup BUFFER where visible, else at the bottom, and select it."
  (let ((win (or (display-buffer-reuse-window buffer alist)
                 (popper-display-popup-at-bottom
                  buffer (append alist '((window-parameters . ((no-other-window . t)))))))))
    (when (window-parameter win 'window-side)
      (adh--popper-window-height win))
    (select-window win)))

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

(defun adh--main-window ()
  "Return the most recently used window that is not a side window."
  (car (sort (seq-remove (lambda (w) (window-parameter w 'window-side))
                         (window-list nil 'nomini))
             (lambda (a b) (> (window-use-time a) (window-use-time b))))))

(defun adh-delete-other-windows ()
  "Delete other windows, keeping side panels such as treemacs.
A popup stays a popup; from a side panel, the last used window stays."
  (interactive)
  (let ((win (selected-window)))
    (if (not (window-parameter win 'window-side))
        (delete-other-windows win)
      (let ((main (adh--main-window)))
        (unless (window-parameter win 'no-delete-other-windows)
          (set-window-dedicated-p main nil)
          (set-window-buffer main (window-buffer win)))
        (delete-other-windows main)
        (select-window (if (window-live-p win) win main))))))

(defun adh-switch-buffer-of-mode (mode prompt)
  "Switch to a buffer whose major mode derives from MODE."
  (let ((names (mapcar #'buffer-name
                       (match-buffers `(derived-mode . ,mode)))))
    (cond ((null names) (user-error "No %s buffers" mode))
          (t (switch-to-buffer
              (completing-read prompt names nil t nil nil (car names)))))))

(defun adh--buffer-listable-p (buf)
  "Return non-nil when BUF should appear in buffer switching."
  (and (buffer-live-p buf)
       (not (string-match-p "\\`[ *]" (buffer-name buf)))
       (not (apply #'provided-mode-derived-p
                   (buffer-local-value 'major-mode buf)
                   adh-hidden-buffer-modes))))

(defun adh-switch-to-buffer ()
  "Switch to a buffer, hiding internal ones and `adh-hidden-buffer-modes'."
  (interactive)
  (switch-to-buffer
   (read-buffer "Buffer: " nil t
                #'(lambda (arg) (adh--buffer-listable-p (cdr arg))))))

(defun adh-kill-other-buffers ()
  "Kill every buffer except visible ones, *scratch*, *Messages* and internals."
  (interactive)
  (let ((keep (append (mapcar #'window-buffer (window-list-1 nil nil t))
                      (list (get-buffer "*scratch*")
                            (get-buffer "*Messages*"))))
        (count 0))
    (dolist (buf (buffer-list))
      (unless (or (memq buf keep)
                  (string-prefix-p " " (buffer-name buf)))
        (when (kill-buffer buf)
          (setq count (1+ count)))))
    (message "Killed %d buffer(s)." count)))

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

(defun adh--dired-marked-files ()
  "Return the files explicitly marked in Dired, or nil when none are."
  (when (derived-mode-p 'dired-mode)
    (let ((files (dired-get-marked-files nil nil nil t)))
      (cond ((eq (car files) t) (cdr files))
            ((cdr files) files)))))

(defun adh--copy-marked (files)
  "Copy FILES to the kill ring, separated by spaces, and return the string."
  (let ((str (mapconcat #'identity files " ")))
    (kill-new str)
    (message "Copied %d files: %s" (length files) str)
    str))

(defun adh-copy-file-name (&optional marked)
  "Copy the file name, or buffer name, to the kill ring.
With MARKED, copy the names of marked Dired files, if any."
  (interactive (list t))
  (let (files f)
    (cond
     ((setq files (and marked (adh--dired-marked-files)))
      (adh--copy-marked (mapcar #'file-name-nondirectory files)))
     ((setq f (adh--buffer-file-name))
      (let ((name (file-name-nondirectory f)))
        (kill-new name)
        (message "Copied %s" name)
        name))
     (t
      (let ((name (buffer-name)))
        (kill-new name)
        (message "Copied buffer name %s" name)
        name)))))

(defun adh-copy-path (&optional resolve)
  "Copy the file's directory to the kill ring; RESOLVE follows symlinks."
  (interactive "P")
  (let* ((file (or (adh--buffer-file-name) default-directory))
         (dir  (file-name-directory (expand-file-name file)))
         (path (adh--maybe-truename (directory-file-name dir) resolve)))
    (kill-new path)
    (message "Copied %s" path)
    path))

(defun adh-copy-full-path (&optional resolve marked)
  "Copy the file's full path to the kill ring; RESOLVE follows symlinks.
With MARKED, copy the paths of marked Dired files, if any."
  (interactive (list current-prefix-arg t))
  (if-let* ((files (and marked (adh--dired-marked-files))))
      (adh--copy-marked (mapcar (lambda (f) (adh--maybe-truename f resolve)) files))
    (let* ((file (or (adh--buffer-file-name) default-directory))
           (path (adh--maybe-truename (expand-file-name file) resolve)))
      (kill-new path)
      (message "Copied %s" path)
      path)))

(defun adh--dired-sort-toggle-or-edit-windows ()
  "`dired-sort-toggle-or-edit' via external ls; ls-lisp ignores sort switches."
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

;; Windows needs the external ls for sort switches.
(defalias 'adh-dired-sort-toggle-or-edit
  (if (eq system-type 'windows-nt)
      #'adh--dired-sort-toggle-or-edit-windows
    #'dired-sort-toggle-or-edit))

(defun adh--apply-window-decoration (decorated)
  "Show or hide the window-manager frame decorations per DECORATED."
  (adh--apply-frame-parameter 'undecorated (not decorated)))

(defun adh--apply-frame-opacity (opacity)
  "Set frame OPACITY (0-100); background only, except on Windows and macOS."
  (adh--apply-frame-parameter
   ;; `alpha' is a window-manager hint; Hyprland ignores it.
   (if (memq system-type '(windows-nt darwin)) 'alpha 'alpha-background)
   opacity))

(defun adh--apply-list-max-height (lines)
  "Show at most LINES lines in vertico, the *Completions* list and popups."
  (setq vertico-count lines
        completions-max-height lines))

(defun adh--apply-font-settings (&rest _)
  "Apply `adh-mono-spaced-font' and its size, also to future frames."
  (let ((family adh-mono-spaced-font)
        (height adh-mono-spaced-font-size))
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
        (message "[adh] Queued font '%s'" family)))))

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

(defun adh-add-root-marker (name)
  "Add NAME to `adh-project-root-markers'."
  (add-to-list 'adh-project-root-markers name))

(defun adh-get-executable ()
  "Get an executable on PATH and jump to it in Dired."
  (interactive)
  (let* ((exec-path (if (eq system-type 'windows-nt) (remove "." exec-path) exec-path))
         (completion-extra-properties
          `(:group-function
            ,(lambda (cand transform)
               (if transform cand
                 (if-let* ((path (locate-file cand exec-path '() 'file-executable-p)))
                     (file-name-directory path) "non-executable")))))
         (exe (completing-read "Exe: " (apply-partially #'locate-file-completion-table exec-path '()) nil t)))
    (dired-jump nil (locate-file exe exec-path '()))))

(defun adh-getenv ()
  "Show an environment variable and copy its value to the kill ring."
  (interactive)
  (let ((val (call-interactively #'getenv)))
    (when val (kill-new val))))

(defalias 'adh-setenv #'setenv)

(defun adh-upgrade-packages (&optional query)
  "Upgrade all packages, including those installed with package-vc."
  (interactive (list t))
  (package-upgrade-all query)
  (package-vc-upgrade-all))

(defun adh--tmux-capture (buffer &optional history)
  "Capture the tmux pane into BUFFER, with its full HISTORY when non-nil."
  (let ((content (shell-command-to-string
                  (format "%s capture-pane -p%s" adh--tmux-command (if history " -S -" "")))))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert content))
    (switch-to-buffer buffer)
    (goto-char (point-max))
    (skip-chars-backward " \t\n")))

(defun adh-tmux-to-emacs-buffer ()
  "Capture the visible tmux pane into the *tmux* buffer."
  (interactive)
  (adh--tmux-capture "*tmux*"))

(defun adh-tmux-to-emacs-buffer-all ()
  "Capture the full tmux pane history into the *tmux-all* buffer."
  (interactive)
  (adh--tmux-capture "*tmux-all*" t))

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
  "Signal a chosen process: SIGTERM, or SIGKILL with a prefix arg."
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

(defvar adh--command-origin-dir nil
  "Directory a project command was invoked from, before moving to the root.")

(defun adh-shell-command-dir-pivot ()
  "Rerun the current compile or shell command prompt in another directory."
  (interactive)
  (let ((input (minibuffer-contents-no-properties))
        (dir (or adh--command-origin-dir default-directory))
        (command (pcase (minibuffer-prompt)
                   ((rx bos "Compile") #'compile)
                   ((rx bos "Async shell command") #'async-shell-command)
                   ((rx bos "Shell command") #'shell-command)
                   (_ (user-error "Not in a compile or shell command prompt")))))
    (run-with-idle-timer adh--minibuffer-pivot-delay nil
     (lambda ()
       (let ((default-directory (let ((use-dialog-box nil))
                                  (expand-file-name (read-directory-name "Run in: " dir nil t)))))
         (minibuffer-with-setup-hook
             (lambda ()
               (delete-minibuffer-contents)
               (insert input))
           (call-interactively command)))))
    (abort-recursive-edit)))

(defun adh-completion-in-region-isearch()
  "Jump to the *Completions* window and start isearch."
  (interactive)
  (select-window (get-buffer-window "*Completions*" t))
  (isearch-forward))

(defun adh--prescient-remember (candidate)
  "Record CANDIDATE with prescient, when prescient is loaded."
  (when (and candidate (fboundp 'prescient-remember))
    (prescient-remember candidate)))

(defun adh-completion-choose ()
  "Choose the selected or first *Completions* candidate and record it."
  (interactive)
  (let ((candidate (with-minibuffer-completions-window
                     (unless (get-text-property (point) 'completion--string)
                       (first-completion))
                     (car (completion-list-candidate-at-point)))))
    (minibuffer-choose-completion)
    (adh--prescient-remember candidate)))

(defun adh--completions-key (cmd)
  "Return a binding that runs CMD only while *Completions* is visible."
  `(menu-item "" ,cmd :filter ,(lambda (c) (and (minibuffer--completions-visible) c))))

(defun adh--completions-preselect-first ()
  "Treat the first *Completions* candidate as selected."
  (with-current-buffer standard-output
    (when (get-text-property (point-min) 'mouse-face)
      (let ((inhibit-read-only t))
        (put-text-property (point-min) (1+ (point-min)) 'first-completion t)))))

(defun adh-isearch-occur ()
  "Run `occur' on the current isearch and exit isearch."
  (interactive)
  (call-interactively #'isearch-occur)
  (isearch-done))

(defun adh-minibuffer-next-history-or-clear (n)
  "Insert the next history element, or clear the input past the end."
  (interactive "p")
  (condition-case nil
      (next-history-element n)
    (error (delete-minibuffer-contents))))

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

(provide 'adh-functions)
