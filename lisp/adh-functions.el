;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-vars)

(defvar vertico-count)

(defvar ls-lisp-use-insert-directory-program)

(defconst adh--tmux-command (if (eq system-type 'windows-nt) "wsl tmux" "tmux")
  "Shell command used to talk to tmux (via WSL on Windows).")
(defconst adh--fd-program (locate-user-emacs-file (concat "opt/fd/bin/fd" (when (eq system-type 'windows-nt) ".exe")))
  "The fd fork that install.sh builds.")
(defconst adh--minibuffer-pivot-delay 0.000001
  "Idle seconds before `adh--minibuffer-pivot-call' reopens the minibuffer.")
(defvar adh--minibuffer-pivot-depth nil
  "Minibuffer depth of the prompt a pivot opened, while it is active.")
(defvar-local adh--occur-edit-changes nil
  "Change group of the edits made since `occur-edit-mode' began.")
(defvar-local adh--occur-edit-ticks nil
  "Modification ticks of the Occur sources when `occur-edit-mode' began.")
(defvar-keymap adh-override-map
  :doc "Keys above every major and minor mode map, see `adh-override-mode'.")

(defmacro => (&rest body)
  "Wrap BODY in an anonymous interactive command, for inline keybindings."
  `(lambda () (interactive) ,@body))

(defmacro adh-defkeymap (name &rest body)
  "Define keymap NAME with bindings, as in `define-keymap', under a parent.
BODY starts with the keywords :map PARENT and :prefix KEY, then the bindings.
Re-evaluating the form updates NAME in place."
  (declare (indent 1))
  (let (parent key)
    (while (keywordp (car body))
      (pcase (pop body)
        (:map (setq parent (pop body)))
        (:prefix (setq key (pop body)))
        (kw (error "adh-defkeymap: unknown keyword %S" kw))))
    (unless (and parent key)
      (error "adh-defkeymap: %S needs :map and :prefix" name))
    `(progn
       (defvar-keymap ,name)
       (define-keymap :keymap ,name ,@body)
       (keymap-set ,parent ,key ,name))))

(define-minor-mode adh-override-mode
  "Keep `adh-override-map' above every major and minor mode map."
  :global t :init-value t :group 'adhoc)

(add-to-list 'emulation-mode-map-alists `((adh-override-mode . ,adh-override-map)))
(add-hook 'minibuffer-setup-hook (lambda () (setq-local adh-override-mode nil)))

(defun adh--minibuffer-pivot-call (fn)
  "Abort the minibuffer, then call FN once Emacs is idle.
In a prompt that a pivot opened, hand FN back to that pivot instead."
  (if (eql adh--minibuffer-pivot-depth (minibuffer-depth))
      (throw 'adh--minibuffer-pivot fn)
    (run-with-idle-timer adh--minibuffer-pivot-delay nil
     (lambda ()
       (with-local-quit
         (while fn
           (setq fn (catch 'adh--minibuffer-pivot
                      (let ((adh--minibuffer-pivot-depth (1+ (minibuffer-depth))))
                        (funcall fn))
                      nil))))))
    (abort-recursive-edit)))

(defun adh--minibuffer-pivot (command)
  "Abort the minibuffer and reopen it under COMMAND with the same input."
  (let ((input (minibuffer-contents)))
    (adh--minibuffer-pivot-call
     (lambda ()
       (minibuffer-with-setup-hook
           (lambda ()
             (setq this-command command)
             (insert input))
         (call-interactively command))))))

(defun adh--with-saved-window (fn)
  "Call FN interactively without letting it change the selected window."
  (save-selected-window
    (call-interactively fn)))

(defun adh--half-window-height ()
  "Return half the current window's body height, at least 1."
  (max 1 (/ (window-body-height) 2)))

(defun adh--get-project-dir (&optional dir)
  "Return the nearest ancestor of DIR with one of `adh-project-root-markers'."
  (locate-dominating-file (or dir default-directory)
   (lambda (d)
     (seq-some (lambda (marker)
                 (file-exists-p (file-name-concat d marker)))
               adh-project-root-markers))))

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
      (when use-region
        (set-mark (+ new-start mark-offset))
        (setq deactivate-mark nil))
      (goto-char (+ new-start point-offset)))))

(defun adh--popup-buffer-p (buf)
  "Return non-nil if BUF matches an entry of `adh-popup-buffers'."
  (with-current-buffer buf
    (seq-some (lambda (entry)
                (if (stringp entry)
                    (string-match-p entry (buffer-name))
                  (derived-mode-p entry)))
              adh-popup-buffers)))

(defun adh--apply-font (family height &optional frame)
  "Set the default face to FAMILY at HEIGHT if FRAME can display it.
Return nil on a terminal or if the font is missing."
  (when (and (display-graphic-p frame)
             (find-font (font-spec :family family) frame))
    (set-face-attribute 'default nil :family family :height height)
    (set-face-attribute 'fixed-pitch nil :family family)
    t))

(defun adh--apply-frame-parameter (parameter value)
  "Set frame PARAMETER to VALUE for all current and future frames."
  (modify-all-frames-parameters (list (cons parameter value))))

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
                (forward-line (if (bolp) (1- steps) steps))
                (if (eolp) (forward-line 1) (end-of-line)))
            (forward-line (- steps)))
          (set-mark (point)))))))

(defun adh-duplicate-dwim (&optional n)
  "Duplicate the line, or the region's whole lines, N times."
  (interactive "p")
  (when (and (use-region-p) (not (bound-and-true-p rectangle-mark-mode)))
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

(defun adh--indent-line ()
  "Indent the current line for the mode, falling back like TAB where it declines."
  (when (eq (indent-according-to-mode) 'noindent)
    (or (indent--default-inside-comment) (indent-relative))))

(defun adh-insert-line-above ()
  "Open and indent a new line above the current one."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (adh--indent-line))

(defun adh-insert-line-below ()
  "Open and indent a new line below the current one."
  (interactive)
  (end-of-line)
  (newline)
  (adh--indent-line))

(defun adh-join-line-above ()
  "Join the following line to the current one."
  (interactive)
  (join-line -1))

(defun adh-backward-delete-char-dwim ()
  "Delete the active region, or the previous character when there is none."
  (interactive)
  (if (use-region-p)
      (delete-active-region)
    (delete-char -1)))

(defun adh-kill-line-above ()
  "Kill the whole line above point."
  (interactive)
  (unless (= (pos-bol) (point-min))
    (forward-line -1)
    (kill-whole-line)))

(defun adh-kill-region-or-line ()
  "Kill the active region, or to end of line when there is none."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end) 'region)
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
  (dotimes (_ (or n 1))
    (while (condition-case nil
               (progn (down-list 1) nil)
             (error (let ((pt (point)))
                      (ignore-errors
                        (backward-up-list 1)
                        (forward-sexp 1)
                        (skip-syntax-forward " >"))
                      (/= (point) pt)))))))

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

(defun adh--main-window (&optional reusable)
  "Return the most recently used window that is not a side window.
When REUSABLE is non-nil, exclude dedicated windows."
  (car (sort (seq-remove (lambda (w)
                          (or (window-parameter w 'window-side)
                              (and reusable (window-dedicated-p w))))
                         (window-list nil 'nomini))
             (lambda (a b) (> (window-use-time a) (window-use-time b))))))

(defun adh-delete-window ()
  "Delete the selected window, or bury a popup in the sole main window."
  (interactive)
  (if (and (memq (bound-and-true-p popper-popup-status) '(popup user-popup))
           (eq (selected-window) (window-main-window)))
      (quit-window)
    (delete-window)))

(defun adh-delete-other-windows ()
  "Delete other windows, keeping protected side panels.
Maximize a popup in a non-dedicated main window without changing its status.
From a protected side panel, keep the last used main window."
  (interactive)
  (let ((win (selected-window)))
    (if (not (window-parameter win 'window-side))
        (delete-other-windows win)
      (let* ((keep-side (window-parameter win 'no-delete-other-windows))
             (main (or (adh--main-window (not keep-side))
                       (user-error "No suitable main window available"))))
        (unless keep-side
          ;; Let quitting restore even buffers hidden by our switching filter.
          (display-buffer-record-window 'reuse main (window-buffer win))
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
       (not (provided-mode-derived-p (buffer-local-value 'major-mode buf)
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
        (when (and name
                   (not (eq buf (current-buffer)))
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
  (if-let* ((files (and marked (adh--dired-marked-files))))
      (adh--copy-marked (mapcar #'file-name-nondirectory files))
    (let* ((file (adh--buffer-file-name))
           (name (if file (file-name-nondirectory file) (buffer-name))))
      (kill-new name)
      (message (if file "Copied %s" "Copied buffer name %s") name)
      name)))

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

(defun adh--apply-window-decoration (decorated)
  "Show or hide the window-manager frame decorations per DECORATED."
  (adh--apply-frame-parameter 'undecorated (not decorated)))

(defun adh--apply-frame-opacity (opacity)
  "Set frame OPACITY (0-100); background only, except on Windows and macOS."
  (adh--apply-frame-parameter
   ;; `alpha' is a window-manager hint; Hyprland ignores it.
   (if (memq system-type '(windows-nt darwin)) 'alpha 'alpha-background)
   opacity)
  (force-mode-line-update t))

(defun adh--apply-list-max-height (lines)
  "Show at most LINES lines in vertico, the *Completions* list and popups."
  (setq vertico-count lines
        completions-max-height lines))

(defun adh--apply-queued-font (frame)
  "Apply `adh-mono-spaced-font' to new FRAME, unhooking once it succeeds."
  (cond ((adh--apply-font adh-mono-spaced-font adh-mono-spaced-font-size frame)
         (remove-hook 'after-make-frame-functions #'adh--apply-queued-font))
        ((display-graphic-p frame)
         (message "[adh] Warning: Queued font '%s' not found." adh-mono-spaced-font))))

(defun adh--apply-font-settings (&rest _)
  "Apply `adh-mono-spaced-font' and its size, also to future frames."
  (if (adh--apply-font adh-mono-spaced-font adh-mono-spaced-font-size)
      (progn
        (remove-hook 'after-make-frame-functions #'adh--apply-queued-font)
        (message "[adh] Set font '%s'" adh-mono-spaced-font))
    (add-hook 'after-make-frame-functions #'adh--apply-queued-font)
    (if (display-graphic-p)
        (message "[adh] Font not found: '%s'" adh-mono-spaced-font)
      (message "[adh] Queued font '%s'" adh-mono-spaced-font))))

(defun adh-set-file-extension-mode (ext mode)
  "Open files with extension EXT in MODE."
  (add-to-list 'auto-mode-alist (cons (format "\\.%s\\'" (regexp-quote ext)) mode)))

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
         (dirs (make-hash-table :test #'equal))
         (completion-extra-properties
          `(:group-function
            ,(lambda (cand transform)
               (if transform cand
                 (with-memoization (gethash cand dirs)
                   (if-let* ((path (locate-file cand exec-path '() 'file-executable-p)))
                       (file-name-directory path) "non-executable"))))))
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
  (let* ((dir (with-current-buffer (window-buffer (selected-window))
                (or (and buffer-file-name (file-name-directory buffer-file-name))
                    default-directory)))
         (path (cond ((not (eq system-type 'windows-nt)) (expand-file-name dir))
                     ((string-match "^\\([a-zA-Z]\\):" dir)
                      (concat "/" (downcase (match-string 1 dir)) (substring dir 2)))
                     (t dir)))
         (dest (or target adh-tmux-cd-session)))
    (start-process-shell-command
     "adh-tmux-cd" nil
     (format "%s send-keys%s 'cd %s' C-m" adh--tmux-command
             (if dest (concat " -t " (shell-quote-argument dest)) "")
             (shell-quote-argument (directory-file-name path))))
    (when (called-interactively-p 'interactive)
      (message "tmux directory -> '%s'" path))
    path))

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
                  (format "Kill (%s): " (if kill "KILL" "TERM"))
                  cands nil t))
         (pid (cdr (assoc choice cands))))
    (when pid
      (let ((sig (if kill 'SIGKILL 'SIGTERM)))
        (signal-process pid sig)
        (message "Sent %s to %d" sig pid)))))

(defvar adh--command-origin-dir nil
  "Directory a project command was invoked from, before moving to the root.")

(defvar adh--shell-prompt-dir nil
  "When set, adh labels compile and shell prompts with this directory.")

(defun adh--origin-dir ()
  "Directory the current command started from, before any move to a root.
Falls back to the directory of the file or Dired buffer the minibuffer serves."
  (or adh--command-origin-dir
      (with-current-buffer (window-buffer (or (minibuffer-selected-window) (selected-window)))
        (cond (buffer-file-name (file-name-directory buffer-file-name))
              ((derived-mode-p 'dired-mode) (dired-current-directory))
              (t default-directory)))))

(defun adh-shell-command-dir-pivot (&optional dir)
  "Rerun the current compile or shell command prompt in DIR, or one read."
  (interactive)
  (let ((input (minibuffer-contents-no-properties))
        (origin (adh--origin-dir))
        (command (pcase (minibuffer-prompt)
                   ((rx bos "Compile" (or " command: " " (")) #'compile)
                   ((rx bos "Async shell" (or " command: " " command in " " (")) #'async-shell-command)
                   ((rx bos "Shell" (or " command: " " command in " " (")) #'shell-command)
                   (_ (user-error "No directory pivot for this prompt")))))
    (adh--minibuffer-pivot-call
     (lambda ()
       (let* ((adh--command-origin-dir origin)
              (default-directory (or dir
                                     (let ((use-dialog-box nil))
                                       (expand-file-name (read-directory-name "Run in: " origin nil t)))))
              (adh--shell-prompt-dir default-directory))
         (minibuffer-with-setup-hook
             (lambda ()
               (delete-minibuffer-contents)
               (insert input))
           (call-interactively command)))))))

(defun adh-shell-command-root-pivot ()
  "Rerun the current compile or shell command prompt at the project root.
When it already runs there, rerun it in the directory it started from."
  (interactive)
  (let* ((origin (adh--origin-dir))
         (root (adh--get-project-dir origin))
         (dir (if (and root (not (file-equal-p default-directory root))) root origin)))
    (if (file-equal-p dir default-directory)
        (user-error (if root "Already at the project root" "Not in a project"))
      (adh-shell-command-dir-pivot dir))))

(defun adh-completion-in-region-isearch()
  "Jump to the *Completions* window and start isearch."
  (interactive)
  (select-window (get-buffer-window "*Completions*" t))
  (isearch-forward))

(defun adh-completion-choose ()
  "Choose the selected or first *Completions* candidate and record it."
  (interactive)
  (let ((candidate (with-minibuffer-completions-window
                     (unless (get-text-property (point) 'completion--string)
                       (first-completion))
                     (car (completion-list-candidate-at-point)))))
    (minibuffer-choose-completion)
    (when (and candidate (fboundp 'prescient-remember))
      (prescient-remember candidate))))

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
  "Record this `occur-edit-mode' session's edits, also in the sources, for undo."
  (dolist (buf (buffer-list))
    (when (buffer-local-value 'adh--occur-edit-changes buf)
      (with-current-buffer buf
        (occur-mode)
        (set-buffer-modified-p nil))))
  (let ((bufs (adh--occur-source-buffers)))
    (setq adh--occur-edit-ticks
          (mapcar (lambda (buf) (cons buf (buffer-chars-modified-tick buf))) bufs)
          adh--occur-edit-changes
          (mapcan #'prepare-change-group
                  (cons (current-buffer)
                        (seq-remove (lambda (buf)
                                      (or (buffer-local-value 'buffer-read-only buf)
                                          (get-buffer-process buf)))
                                    bufs)))))
  (activate-change-group adh--occur-edit-changes)
  (dolist (elt adh--occur-edit-changes)
    (with-current-buffer (car elt)
      (setq-local undo-limit most-positive-fixnum
                  undo-strong-limit most-positive-fixnum
                  undo-outer-limit nil)))
  (add-hook 'after-change-functions #'adh--occur-edit-plain-input nil t)
  (add-hook 'change-major-mode-hook #'adh--occur-edit-end nil t)
  (add-hook 'kill-buffer-hook #'adh--occur-edit-end nil t))

(add-hook 'occur-edit-mode-hook #'adh--occur-edit-start)

(defun adh--occur-edit-end (&optional abort)
  "Close the change group of this `occur-edit-mode' session, undoing it when ABORT."
  (let ((changes adh--occur-edit-changes)
        (after-change-functions nil))
    (setq adh--occur-edit-changes nil)
    (dolist (elt changes)
      (when (buffer-live-p (car elt))
        (with-demoted-errors "[adh] Occur edit: %S"
          (if abort (cancel-change-group (list elt)) (accept-change-group (list elt))))
        (with-current-buffer (car elt)
          (mapc #'kill-local-variable '(undo-limit undo-strong-limit undo-outer-limit)))))))

(defun adh-occur-edit-abort ()
  "Undo the edits made in `occur-edit-mode', also in the sources, and leave it."
  (interactive)
  (adh--occur-edit-end t)
  (occur-cease-edit)
  (set-buffer-modified-p nil))

(defun adh-occur-edit-save ()
  "Leave `occur-edit-mode' and save the source files edited in it."
  (interactive)
  (let ((ticks adh--occur-edit-ticks))
    (occur-cease-edit)
    (set-buffer-modified-p nil)
    (dolist (tick ticks)
      (let ((buf (car tick)))
        (when (and (buffer-live-p buf) (buffer-file-name buf) (buffer-modified-p buf)
                   (/= (cdr tick) (buffer-chars-modified-tick buf)))
          (with-current-buffer buf (save-buffer)))))))

(defun adh--pcre-quote (string)
  "Escape STRING to match literally as a PCRE or Python regexp."
  (replace-regexp-in-string (rx (any "\\.^$|?*+()[]{}")) "\\\\\\&" string))

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
