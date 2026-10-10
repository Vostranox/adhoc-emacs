;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar vertico--index)
(defvar vertico--candidates)
(defvar vertico-multiform-commands)
(defvar vertico-multiform-categories)
(defvar vertico-multiform--display-modes)
(defvar vertico-multiform-mode)
(defvar vertico-buffer-mode)
(defvar vertico-buffer-display-action)
(defvar vertico-buffer-hide-prompt)
(defvar marginalia-annotators)
(defvar adh-use-picker)

(declare-function vertico--candidate "vertico")
(declare-function vertico--metadata-get "vertico")
(declare-function vertico-buffer-mode "vertico-buffer")
(declare-function bookmark-get-filename "bookmark")
(declare-function bookmark-get-position "bookmark")
(declare-function xref-item-location "xref" t t)
(declare-function xref-file-location-file "xref" t t)
(declare-function xref-file-location-line "xref" t t)
(declare-function xref-buffer-location-buffer "xref" t t)
(declare-function xref-buffer-location-position "xref" t t)
(declare-function xref-location-group "xref")
(declare-function xref-location-line "xref")
(declare-function xref-elisp-location-file "elisp-mode" t t)
(declare-function xref-elisp-location-symbol "elisp-mode" t t)
(declare-function flymake-diagnostic-buffer "flymake" t t)
(declare-function flymake-diagnostic-beg "flymake" t t)

;;;; Options

(defgroup adh-picker nil
  "Floating picker that previews the candidates of Vertico prompts."
  :group 'adhoc
  :prefix "adh-picker-")

(defcustom adh-picker-commands
  '(consult-buffer consult-project-buffer adh-consult-buffer
    consult-fd consult-find consult-locate consult-recent-file project-find-file
    adh-consult-fd-here adh-consult-fd-project adh-consult-fd-dirs adh-consult-locate
    consult-bookmark
    adh-consult-zoxide adh-switch-dired-dwim dirvish-history-jump
    consult-ripgrep consult-grep consult-git-grep
    adh-consult-ripgrep-here adh-consult-ripgrep-project adh-consult-ripgrep-dirs
    consult-line consult-line-multi consult-outline consult-mark consult-global-mark
    consult-imenu consult-imenu-multi
    consult-xref
    consult-flycheck consult-flymake consult-compile-error
    project-switch-project describe-symbol describe-function describe-variable
    adh-consult-info
    adh-git-log adh-git-log-file adh-git-log-line adh-git-status adh-git-hunks
    adh-git-branches adh-git-stash)
  "Commands whose prompts open in the floating picker."
  :group 'adh-picker
  :type '(repeat (function :tag "Command"))
  :initialize #'custom-initialize-default
  :set #'adh--picker-set-commands)

(defcustom adh-picker-size '(0.9 . 0.8)
  "Width and height of the picker, as fractions of the frame."
  :group 'adh-picker
  :type '(cons float float))

(defcustom adh-picker-list-ratio 0.45
  "Fraction of the picker width taken by the candidate list."
  :group 'adh-picker
  :type 'float)

(defcustom adh-picker-plain-categories '(multi-category file project-file buffer)
  "Completion categories listed without Marginalia annotations in the picker."
  :group 'adh-picker
  :type '(repeat symbol))

(defcustom adh-picker-preview-max-size (* 512 1024)
  "Maximum uncompressed bytes previewed from a file that is not visited."
  :group 'adh-picker
  :type 'natnum)

(defcustom adh-picker-preview-match-limit 128
  "Maximum number of match overlays in a picker preview."
  :group 'adh-picker
  :type 'natnum)

(defcustom adh-picker-preview-delay 0.05
  "Seconds to wait before previewing a file that has not been read yet."
  :group 'adh-picker
  :type 'number)

(defcustom adh-picker-preview-cache 8
  "Number of files whose preview buffers are kept for reuse."
  :group 'adh-picker
  :type 'natnum)

(defface adh-picker-border
  '((t (:inherit shadow)))
  "Face for the border and the divider of the picker."
  :group 'adh-picker)

(defface adh-picker-preview-match
  '((t (:inherit lazy-highlight)))
  "Face for the input's matches on the line the picker preview points at."
  :group 'adh-picker)

(defface adh-picker-preview-line
  '((t (:inherit highlight :extend t)))
  "Face for the line the picker preview points at."
  :group 'adh-picker)

(defvar adh-picker-previews nil
  "Preview functions by completion category; see `adh-picker-define-preview'.")

(defvar adh--picker-frame nil
  "Child frame holding the candidate list and the preview.")

(defvar adh--picker-list-window nil
  "Window of `adh--picker-frame' that shows the candidates.")

(defvar adh--picker-preview-window nil
  "Window of `adh--picker-frame' that shows the preview.")

(defvar adh--picker-geometry nil
  "Cached picker layout; see `adh--picker-geometry'.")

(defvar adh--picker-overlay nil
  "Overlay marking the previewed line, visible only in the preview window.")

(defvar adh--picker-match-overlays nil
  "Overlays marking the input's matches in the preview window.")

(defvar adh--picker-timer nil)
(defvar adh--picker-idle-timer nil
  "Timer that updates the preview once pending input has been handled.")

(defvar adh--picker-shown 'none
  "Target the preview currently shows, consed to the matches it highlights.")

(defvar adh--picker-active nil
  "Non-nil while the current prompt runs in the picker frame.")

(defvar adh--picker-owns-buffer-mode nil
  "Non-nil when the picker turned on `vertico-buffer-mode'.")

(defvar adh--picker-fallback-modes nil
  "Default layout modes the picker enabled because it could not float.")

(defvar adh--picker-file-buffers nil
  "Cached file previews, most recent first, as (FILE MODTIME . BUFFER).")

(defvar adh--picker-indirect-buffers nil
  "Pairs of narrowed source buffers and widened indirect previews.
These share live text with their sources and are discarded on closing.")

(defvar-local adh--picker-partial nil
  "Non-nil in a file preview that holds only the start of its file.")

(defvar adh--picker-imenu-items nil
  "Items of the last `consult-imenu' prompt, which hold their positions.")

(defconst adh--picker-match-faces
  '(completions-common-part consult-highlight-match xref-match
                            orderless-match-face-0 orderless-match-face-1
                            orderless-match-face-2 orderless-match-face-3)
  "Faces that mark the parts of a candidate matching the input.")

(defconst adh--picker-local-vars
  '(vertico-buffer-display-action vertico-buffer-hide-prompt mode-line-format
                                  marginalia-field-width marginalia-annotators)
  "Variables the picker sets locally in the minibuffer.")

(defun adh--picker-parent ()
  "Return the frame the picker floats over."
  (let* ((win (minibuffer-selected-window))
         (frame (if (window-live-p win) (window-frame win) (selected-frame))))
    (while (frame-parent frame)
      (setq frame (frame-parent frame)))
    frame))

(defun adh--picker-geometry ()
  "Return the picker layout over the parent frame as a plist."
  (let* ((frame (adh--picker-parent))
         (cw (frame-char-width frame))
         (ch (frame-char-height frame))
         (fw (frame-pixel-width frame))
         (fh (frame-pixel-height frame))
         (key (list frame fw fh cw ch adh-picker-size adh-picker-list-ratio)))
    (if (equal key (plist-get adh--picker-geometry :key))
        adh--picker-geometry
      (let* ((cols (max 40 (floor (* (/ fw cw) (car adh-picker-size)))))
             (rows (max 6 (floor (* (/ fh ch) (cdr adh-picker-size)))))
             (border 1))
        (setq adh--picker-geometry
              (list :key key :frame frame :cols cols :rows rows :border border
                    :list-cols (max 20 (round (* cols adh-picker-list-ratio)))
                    :x (max 0 (/ (- fw (* cols cw) (* 2 border)) 2))
                    :y (max 0 (/ (- fh (* rows ch) (* 2 border)) 2))))))))

(defun adh--picker-tty-borders ()
  "Draw terminal child frame borders with light rounded box-drawing characters.
Slots the user already set in `standard-display-table' are kept."
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))
  (pcase-dolist (`(,slot . ,char) '((box-vertical . ?│) (box-horizontal . ?─)
                                    (box-down-right . ?╭) (box-down-left . ?╮)
                                    (box-up-right . ?╰) (box-up-left . ?╯)))
    (unless (display-table-slot standard-display-table slot)
      (set-display-table-slot standard-display-table slot
                              (make-glyph-code char 'adh-picker-border)))))

(defun adh--picker-scratch ()
  "Return the buffer that holds directory listings and messages for the preview."
  (or (get-buffer " *adh-picker-preview*")
      (with-current-buffer (get-buffer-create " *adh-picker-preview*" t)
        (setq buffer-read-only t)
        (current-buffer))))

(defun adh--picker-make-frame (parent border)
  "Return a hidden child frame of PARENT for the picker, with BORDER pixels."
  (let* ((graphic (display-graphic-p parent))
         (after-make-frame-functions nil)
         (frame (make-frame
                 `((parent-frame . ,parent)
                   (minibuffer . ,(minibuffer-window parent))
                   (title . "adh-picker")
                   (visibility . nil)
                   (fullscreen . nil)
                   (no-accept-focus . t)
                   (no-focus-on-map . t)
                   (no-other-frame . t)
                   (unsplittable . t)
                   (undecorated . ,graphic)
                   (border-width . 0)
                   (internal-border-width . ,border)
                   (child-frame-border-width . ,border)
                   (right-divider-width . ,(if graphic border 0))
                   (bottom-divider-width . 0)
                   (vertical-scroll-bars . nil)
                   (horizontal-scroll-bars . nil)
                   (left-fringe . 0)
                   (right-fringe . 0)
                   (menu-bar-lines . 0)
                   (tool-bar-lines . 0)
                   (tab-bar-lines . 0)
                   (line-spacing . 0)
                   (min-width . 0)
                   (min-height . 0)
                   (width . 40)
                   (height . 6)
                   (cursor-type . nil)
                   (tty-non-selected-cursor . nil)
                   (no-special-glyphs . t)
                   (desktop-dont-save . t)))))
    (set-window-buffer (frame-root-window frame) (adh--picker-scratch))
    (redirect-frame-focus frame parent)
    frame))

(defun adh--picker-refresh-style (frame parent)
  "Give FRAME the font of PARENT and the color of `adh-picker-border'.
Only what changed since the last prompt is set, e.g. after a theme switch."
  (let ((font (and (display-graphic-p parent) (face-attribute 'default :font parent)))
        (color (face-foreground 'adh-picker-border nil t)))
    (when (and font (not (equal font (frame-parameter frame 'adh-picker-font))))
      (set-frame-parameter frame 'font font)
      (set-frame-parameter frame 'adh-picker-font font)
      (set-frame-parameter frame 'adh-picker-geometry nil))
    (when (and color (not (equal color (frame-parameter frame 'adh-picker-color))))
      (set-face-background (if (facep 'child-frame-border) 'child-frame-border 'internal-border)
                           color frame)
      (dolist (face '(vertical-border window-divider
                                      window-divider-first-pixel window-divider-last-pixel))
        (set-face-foreground face color frame))
      (set-frame-parameter frame 'adh-picker-color color))))

(defun adh--picker-redirect-focus ()
  "Give focus back to the parent frame if the picker frame got it."
  (when (and (frame-live-p adh--picker-frame)
             (eq (selected-frame) adh--picker-frame))
    (let ((parent (frame-parent adh--picker-frame)))
      (redirect-frame-focus adh--picker-frame parent)
      (select-frame-set-input-focus parent))))

(defun adh--picker-divider-table ()
  "Return a display table drawing the list/preview divider as a light line."
  (let ((table (make-display-table)))
    (set-display-table-slot table 'vertical-border (make-glyph-code ?│ 'adh-picker-border))
    table))

(defun adh--picker-setup-frame (geo)
  "Size, place and split the picker frame for layout GEO; keep it hidden."
  (let ((parent (plist-get geo :frame)))
    (unless (and (frame-live-p adh--picker-frame)
                 (eq (frame-parent adh--picker-frame) parent))
      (when (frame-live-p adh--picker-frame)
        (delete-frame adh--picker-frame))
      (setq adh--picker-frame (adh--picker-make-frame parent (plist-get geo :border)))))
  (let ((frame adh--picker-frame))
    (adh--picker-refresh-style frame (plist-get geo :frame))
    (unless (eq (frame-parameter frame 'adh-picker-geometry) geo)
      (set-frame-size frame (plist-get geo :cols) (plist-get geo :rows))
      (set-frame-position frame (plist-get geo :x) (plist-get geo :y))
      (set-frame-parameter frame 'adh-picker-geometry geo)
      (setq adh--picker-list-window nil))
    (unless (and (window-live-p adh--picker-list-window)
                 (window-live-p adh--picker-preview-window)
                 (eq (window-frame adh--picker-list-window) frame)
                 (eq (window-frame adh--picker-preview-window) frame))
      (let ((ignore-window-parameters t)
            (preview (frame-first-window frame)))
        (set-window-dedicated-p preview nil)
        (delete-other-windows preview)
        (set-window-buffer preview (adh--picker-scratch))
        (setq adh--picker-preview-window preview
              adh--picker-list-window
              (split-window preview (- (plist-get geo :list-cols)) 'left))
        (dolist (win (list adh--picker-list-window preview))
          (set-window-parameter win 'mode-line-format 'none)
          (set-window-parameter win 'no-other-window t))
        (set-window-parameter adh--picker-list-window 'header-line-format 'none)
        (set-window-display-table adh--picker-list-window (adh--picker-divider-table))))))

(defun adh--picker-display-list (buffer _alist)
  "Display action for `vertico-buffer-mode': show BUFFER in the picker list."
  (when (window-live-p adh--picker-list-window)
    (set-window-buffer adh--picker-list-window buffer)
    adh--picker-list-window))

(defun adh--picker-hide ()
  "Hide the picker and drop its references to previewed buffers."
  (when (timerp adh--picker-timer)
    (cancel-timer adh--picker-timer)
    (setq adh--picker-timer nil))
  (when (timerp adh--picker-idle-timer)
    (cancel-timer adh--picker-idle-timer)
    (setq adh--picker-idle-timer nil))
  (when (overlayp adh--picker-overlay)
    (delete-overlay adh--picker-overlay))
  (adh--picker-clear-matches)
  (when (and adh--picker-preview-window
             (eq minibuffer-scroll-window adh--picker-preview-window))
    (setq minibuffer-scroll-window nil))
  (when (frame-live-p adh--picker-frame)
    (make-frame-invisible adh--picker-frame t))
  (when (window-live-p adh--picker-preview-window)
    (set-window-buffer adh--picker-preview-window (adh--picker-scratch)))
  (dolist (entry adh--picker-indirect-buffers)
    (when (buffer-live-p (cdr entry))
      (kill-buffer (cdr entry))))
  (setq adh--picker-indirect-buffers nil)
  (setq adh--picker-shown 'none))

(defun adh--picker-fill (fn)
  "Reset the scratch preview buffer and call FN in it; return the buffer."
  (with-current-buffer (adh--picker-scratch)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (delay-mode-hooks (fundamental-mode))
      (funcall fn)
      (setq-local truncate-lines t)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (current-buffer))))

(defun adh--picker-text (text)
  "Return the scratch buffer showing TEXT."
  (adh--picker-fill (lambda () (insert text))))

(defun adh--picker-insert-gzip (file)
  "Insert bounded uncompressed contents of FILE; return non-nil if truncated.
Stop the external decoder at the preview limit or after one second."
  (let ((program (or (executable-find "gzip")
                     (error "Install gzip to preview compressed files")))
        (file (expand-file-name file))
        (destination (current-buffer))
        (limit (+ 4 adh-picker-preview-max-size))
        (deadline (+ (float-time) 1.0))
        (errors (generate-new-buffer " *adh-picker-gzip-errors*"))
        proc)
    (unwind-protect
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (let ((output (current-buffer))
                (default-directory temporary-file-directory))
            (setq proc
                  (make-process
                   :name "adh-picker-gzip" :buffer nil :noquery t
                   :connection-type 'pipe :coding 'no-conversion :stderr errors
                   :command (list program "-cd" "--" file)
                   :sentinel #'ignore
                   :filter (lambda (process chunk)
                             (with-current-buffer output
                               (insert (substring chunk 0 (min (length chunk)
                                                               (- limit (buffer-size)))))
                               (when (>= (buffer-size) limit)
                                 (delete-process process)))))))
          (while (and (process-live-p proc) (< (float-time) deadline))
            (accept-process-output proc 0.01))
          (when (process-live-p proc)
            (error "Compressed preview timed out"))
          (let ((partial (> (buffer-size) adh-picker-preview-max-size)))
            (unless (or partial (eq (process-exit-status proc) 0))
              (error "Cannot decompress preview: %s"
                     (with-current-buffer errors (string-trim (buffer-string)))))
            (let* ((raw (buffer-string))
                   (requested (or coding-system-for-read 'undecided))
                   (coding requested))
              (when (eq (coding-system-type requested) 'undecided)
                (setq coding (detect-coding-string raw t))
                (when (and (= (length raw) limit)
                           (not (eq (coding-system-type coding) 'utf-8)))
                  (setq coding
                        (or (cl-loop for trim from 1 to 3
                                     for candidate = (detect-coding-string
                                                      (substring raw 0 (- (length raw) trim)) t)
                                     when (eq (coding-system-type candidate) 'utf-8)
                                     return candidate)
                            coding)))
                (when (integerp (coding-system-eol-type requested))
                  (setq coding (coding-system-change-eol-conversion
                                coding (coding-system-eol-type requested)))))
              (let* ((text (decode-coding-string
                            (substring raw 0 (min (length raw) adh-picker-preview-max-size))
                            coding))
                     (end (length text)))
                (when (and partial (eq (coding-system-type coding) 'utf-8))
                  (while (and (> end 0) (> (aref text (1- end)) #x10ffff))
                    (setq end (1- end))))
                (with-current-buffer destination (insert (substring text 0 end)))))
            partial))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (kill-buffer errors))))

(defun adh--picker-read-file (file)
  "Insert the start of FILE into the current buffer, set up for display."
  (let (partial)
    (condition-case err
        (progn
          (let ((size (or (file-attribute-size (file-attributes file)) 0))
                (file-name-handler-alist nil))
            (if (not (string-suffix-p ".gz" file))
                (progn
                  (insert-file-contents file nil 0 adh-picker-preview-max-size)
                  (setq partial (> size adh-picker-preview-max-size)))
              (setq partial (adh--picker-insert-gzip file))))
          (if (save-excursion (goto-char (point-min))
                              (search-forward "\0" (min (point-max) 8000) t))
              (progn (erase-buffer) (insert "Binary file"))
            (let ((buffer-file-name file))
              (delay-mode-hooks (ignore-errors (set-auto-mode))))
            (ignore-errors
              (font-lock-set-defaults)
              (jit-lock-register #'font-lock-fontify-region))))
      (error (setq partial nil) (erase-buffer) (insert (error-message-string err))))
    (setq-local adh--picker-partial partial))
  (setq-local truncate-lines t)
  (setq buffer-read-only t))

(defun adh--picker-cached-file (file)
  "Return the cached preview buffer of FILE if it is still current."
  (when-let* ((hit (assoc file adh--picker-file-buffers))
              ((buffer-live-p (cddr hit)))
              ((equal (cadr hit) (file-attribute-modification-time (file-attributes file)))))
    (cddr hit)))

(defun adh--picker-file-buffer (file)
  "Return a buffer with FILE's contents, reusing a cached one when current."
  (let ((hit (assoc file adh--picker-file-buffers)))
    (if-let* ((buf (adh--picker-cached-file file)))
        (progn
          (setq adh--picker-file-buffers (cons hit (delq hit adh--picker-file-buffers)))
          buf)
      (when hit
        (kill-buffer (cddr hit))
        (setq adh--picker-file-buffers (delq hit adh--picker-file-buffers)))
      (let ((buf (generate-new-buffer " *adh-picker-file*" t)))
        (with-current-buffer buf
          (adh--picker-read-file file))
        (push (cons file (cons (file-attribute-modification-time (file-attributes file)) buf))
              adh--picker-file-buffers)
        (while (> (length adh--picker-file-buffers) (max 1 adh-picker-preview-cache))
          (kill-buffer (cddr (car (last adh--picker-file-buffers))))
          (setq adh--picker-file-buffers (butlast adh--picker-file-buffers)))
        buf))))

(defun adh--picker-flush-files ()
  "Kill the cached file preview buffers."
  (dolist (entry adh--picker-file-buffers)
    (when (buffer-live-p (cddr entry))
      (kill-buffer (cddr entry))))
  (setq adh--picker-file-buffers nil))

(defun adh--picker-dir-buffer (dir)
  "Return the scratch buffer listing DIR, directories first."
  (adh--picker-fill
   (lambda ()
     (let* ((face (if (facep 'dired-directory) 'dired-directory 'font-lock-function-name-face))
            (entries (ignore-errors
                       (directory-files-and-attributes
                        dir nil directory-files-no-dot-files-regexp t nil 2000))))
       (dolist (entry (sort entries
                            (lambda (a b)
                              (let ((da (eq (cadr a) t)) (db (eq (cadr b) t)))
                                (if (eq da db) (string< (car a) (car b)) da)))))
         (insert (if (eq (cadr entry) t)
                     (propertize (concat (car entry) "/") 'face face)
                   (car entry))
                 "\n"))))))

(defun adh--picker-search-pos (buffer regexp)
  "Return the start of the first line in BUFFER that matches REGEXP, or nil."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (and (re-search-forward regexp nil t) (pos-bol))))))

(defun adh--picker-line-pos (buffer line)
  "Return the position of LINE in BUFFER."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- (max 1 line)))
        (point)))))

(defun adh--picker-buffer-view (buffer pos)
  "Return BUFFER, widening an indirect view only when POS is inaccessible."
  (with-current-buffer buffer
    (if (or (not pos) (not (buffer-narrowed-p)) (<= (point-min) pos (point-max)))
        buffer
      (let ((view (cdr (assq buffer adh--picker-indirect-buffers))))
        (unless (buffer-live-p view)
          (setq view (make-indirect-buffer buffer
                                          (generate-new-buffer-name " *adh-picker-view*")
                                          t t))
          (push (cons buffer view) adh--picker-indirect-buffers)
          (with-current-buffer view
            (setq-local buffer-read-only t
                        kill-buffer-hook nil
                        kill-buffer-query-functions nil)))
        (with-current-buffer view (widen))
        view))))

(defun adh--picker-visiting (file)
  "Return the buffer visiting FILE, never contacting a remote host."
  (if (file-remote-p file)
      (get-file-buffer file)
    (find-buffer-visiting file)))

(defun adh--picker-quick-p (target)
  "Non-nil if TARGET can be previewed without reading a file."
  (pcase target
    (`(file ,file . ,_)
     (or (file-remote-p file) (adh--picker-visiting file) (adh--picker-cached-file file)))
    (`(custom ,_ ,_ ,_ ,quick) quick)
    (_ t)))

(defun adh--picker-resolve (target)
  "Turn TARGET into a list (BUFFER POS TITLE) to show."
  (pcase target
    (`(buffer ,buf ,pos)
     (list (adh--picker-buffer-view buf pos)
           (or pos (with-current-buffer buf (point))) (buffer-name buf)))
    (`(file ,file ,line . ,search)
     (let ((title (abbreviate-file-name file)))
       (cond
        ((file-remote-p file) (list (adh--picker-text "Remote file") nil title))
        ((file-directory-p file) (list (adh--picker-dir-buffer file) nil title))
        ((not (file-readable-p file)) (list (adh--picker-text "") nil title))
        (t (let* ((buf (or (adh--picker-visiting file) (adh--picker-file-buffer file)))
                  (pos (cond (line (adh--picker-line-pos buf line))
                             (search (adh--picker-search-pos buf (car search))))))
             (list (adh--picker-buffer-view buf pos) pos
                   (if (buffer-local-value 'adh--picker-partial buf)
                       (format "%s  (first %s)" title
                               (file-size-human-readable adh-picker-preview-max-size))
                     title)))))))
    (`(text ,title ,text) (list (adh--picker-text text) nil title))
    (`(custom ,fn ,cand ,dir ,_)
     (let ((title (adh--picker-plain-string cand))
           (preview (let ((default-directory dir)) (funcall fn cand))))
       (pcase preview
         ((pred bufferp) (list preview nil title))
         (`(,(and (pred bufferp) buf) . ,(and (pred integerp) pos))
          (list (adh--picker-buffer-view buf pos) pos title))
         ((pred stringp) (list (adh--picker-text preview) nil title))
         (_ (list (adh--picker-text "") nil title)))))
    (_ (list (adh--picker-text "") nil ""))))

(defun adh--picker-plain-string (cand)
  "Return CAND without text properties or Consult's invisible suffix."
  (string-trim (apply #'string (seq-remove (lambda (char) (>= char #x100000))
                                           (substring-no-properties cand)))))

(defun adh--picker-marker-target (pos)
  "Return a buffer target for POS: a marker, (BUFFER . POS) or (MARKER ...)."
  (pcase pos
    ((and (pred markerp) (guard (marker-buffer pos)))
     (list 'buffer (marker-buffer pos) (marker-position pos)))
    (`(,(and (pred bufferp) buf) . ,(and (pred integerp) p))
     (when (buffer-live-p buf) (list 'buffer buf p)))
    (`(,(and (pred markerp) marker) . ,_) (adh--picker-marker-target marker))))

(defun adh--picker-file-target (name &optional line)
  "Return a file target for NAME at LINE, NAME relative to the prompt's directory."
  (list 'file (expand-file-name name) line))

(defun adh--picker-grep-target (cand)
  "Return a file target for consult grep candidate CAND."
  (when-let* ((file-end (next-single-property-change 0 'face cand))
              (line-end (next-single-property-change (1+ file-end) 'face cand)))
    (adh--picker-file-target
     (substring-no-properties cand 0 file-end)
     (string-to-number (substring-no-properties cand (1+ file-end) line-end)))))

(defun adh--picker-bookmark-target (name)
  "Return a file target for bookmark NAME."
  (when-let* (((require 'bookmark nil t))
              (file (bookmark-get-filename name)))
    (let ((file (expand-file-name file)))
      (if-let* ((buf (adh--picker-visiting file))
                (pos (bookmark-get-position name)))
          (list 'buffer buf (min pos (with-current-buffer buf
                                      (save-restriction (widen) (point-max)))))
        (list 'file file nil)))))

(defun adh--picker-xref-target (item)
  "Return a target for xref ITEM without visiting its file."
  (let ((loc (xref-item-location item)))
    (pcase (type-of loc)
      ('xref-file-location
       (list 'file (xref-file-location-file loc) (xref-file-location-line loc)))
      ('xref-buffer-location
       (list 'buffer (xref-buffer-location-buffer loc) (xref-buffer-location-position loc)))
      ('xref-elisp-location (adh--picker-elisp-target loc))
      (_ (when-let* ((file (ignore-errors (xref-location-group loc)))
                     ((stringp file))
                     ((file-name-absolute-p file)))
           (list 'file file (ignore-errors (xref-location-line loc))))))))

(defun adh--picker-elisp-target (loc)
  "Return a file target for the Emacs Lisp definition LOC."
  (let ((file (xref-elisp-location-file loc))
        (symbol (xref-elisp-location-symbol loc)))
    (when (stringp file)
      (setq file (string-remove-suffix "c" file))
      (unless (file-exists-p file)
        (setq file (concat file ".gz")))
      (list 'file file nil
            (format "^\\s-*(\\(?:\\sw\\|\\s_\\)*def\\(?:\\sw\\|\\s_\\)*\\s-+'?%s\\_>[^)]"
                    (regexp-quote (symbol-name symbol)))))))

(defun adh--picker-remember-imenu (_prompt items)
  "Keep the ITEMS a `consult-imenu' prompt offers, for their previews."
  (setq adh--picker-imenu-items items))

(defun adh--picker-imenu-target (pos)
  "Return a buffer target for Imenu position POS."
  (pcase pos
    ((pred integerp)
     (list 'buffer (window-buffer (minibuffer-selected-window)) pos))
    ((pred markerp) (adh--picker-marker-target pos))
    ((and (pred overlayp) (guard (overlay-buffer pos)))
     (list 'buffer (overlay-buffer pos) (overlay-start pos)))
    (`(,p . ,_) (adh--picker-imenu-target p))))

(defun adh--picker-symbol-target (name)
  "Return a text target with the documentation of the symbol NAME."
  (when-let* ((sym (intern-soft name))
              (doc (ignore-errors
                     (or (and (fboundp sym) (documentation sym t))
                         (documentation-property sym 'variable-documentation t)))))
    (list 'text name doc)))

(defun adh--picker-item-target (category item cand)
  "Return a preview target for ITEM of CATEGORY, displayed as CAND."
  (pcase category
    ('buffer
     (when-let* ((buf (if (bufferp item) item (get-buffer item))))
       (when (buffer-live-p buf) (list 'buffer buf nil))))
    ((or 'file 'project-file) (adh--picker-file-target item))
    ('bookmark (adh--picker-bookmark-target item))
    ('consult-grep (adh--picker-grep-target cand))
    ((or 'command 'function 'variable 'symbol 'symbol-help) (adh--picker-symbol-target item))
    ('imenu (adh--picker-imenu-target (cdr (assoc item adh--picker-imenu-items))))))

(defun adh--picker-custom-target (category cand)
  "Return a target previewing CAND with the preview function of CATEGORY."
  (when-let* ((preview (alist-get category adh-picker-previews)))
    (list 'custom (car preview) cand default-directory (cdr preview))))

(defun adh--picker-match-strings ()
  "Return the parts of the current candidate that match the input."
  (let* ((cand (vertico--candidate t))
         (len (length cand))
         (pos 0)
         strings)
    (while (< pos len)
      (let ((next (next-single-property-change pos 'face cand len)))
        (when (seq-some (lambda (face) (memq face adh--picker-match-faces))
                        (ensure-list (get-text-property pos 'face cand)))
          (let ((match (string-trim (substring-no-properties cand pos next))))
            (when (length> match 1)
              (push match strings))))
        (setq pos next)))
    (delete-dups strings)))

(defun adh--picker-highlight-matches (win strings)
  "Highlight STRINGS near point in WIN with bounded scanning and overlays."
  (let* ((width (min 1024 (max 1 (window-body-width win))))
         (beg (max (pos-bol) (- (point) (* 2 width))))
         (end (min (pos-eol) (+ beg (* 4 width))))
         (remaining adh-picker-preview-match-limit)
         (case-fold-search t))
    (dolist (string strings)
      (unless (string-empty-p string)
        (save-excursion
          (goto-char beg)
          (while (and (> remaining 0) (search-forward string end t))
            (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put ov 'face 'adh-picker-preview-match)
              (overlay-put ov 'window win)
              (overlay-put ov 'priority 1001)
              (push ov adh--picker-match-overlays)
              (setq remaining (1- remaining)))))))))

(defun adh--picker-clear-matches ()
  "Delete the overlays marking the input's matches in the preview."
  (mapc #'delete-overlay adh--picker-match-overlays)
  (setq adh--picker-match-overlays nil))

(defun adh--picker-target ()
  "Describe what the current Vertico candidate points at, or nil."
  (when (>= vertico--index 0)
    (let ((cand (nth vertico--index vertico--candidates))
          (category (vertico--metadata-get 'category)))
      (cond
       ((adh--picker-custom-target category cand))
       ((when-let* ((multi (get-text-property 0 'multi-category cand)))
          (adh--picker-custom-target (car multi) cand)))
       ((get-text-property 0 'consult-location cand)
        (adh--picker-marker-target (car (get-text-property 0 'consult-location cand))))
       ((get-text-property 0 'consult--info cand)
        (pcase-let ((`(,_ ,bol ,buf) (get-text-property 0 'consult--info cand)))
          (adh--picker-marker-target (cons buf bol))))
       ((get-text-property 0 'consult-xref cand)
        (adh--picker-xref-target (get-text-property 0 'consult-xref cand)))
       ((get-text-property 0 'consult--candidate cand)
        (let ((item (get-text-property 0 'consult--candidate cand)))
          (if (and (fboundp 'flymake-diagnostic-p) (flymake-diagnostic-p item))
              (let ((buf (flymake-diagnostic-buffer item))
                    (beg (flymake-diagnostic-beg item)))
                (when (and (bufferp buf) (integerp beg)) (list 'buffer buf beg)))
            (adh--picker-marker-target item))))
       ((get-text-property 0 'multi-category cand)
        (let ((multi (get-text-property 0 'multi-category cand)))
          (adh--picker-item-target (car multi) (cdr multi) cand)))
       (t (adh--picker-item-target category (vertico--candidate) cand))))))

(defun adh--picker-render (target &optional matches)
  "Show TARGET in the preview window and make the picker visible.
MATCHES are strings to highlight on the line TARGET points at."
  (setq adh--picker-timer nil)
  (adh--picker-clear-matches)
  (when (and adh--picker-active
             (active-minibuffer-window)
             (window-live-p adh--picker-preview-window))
    (pcase-let* ((win adh--picker-preview-window)
                 (`(,buf ,pos ,title)
                  (condition-case err
                      (adh--picker-resolve target)
                    (error (list (adh--picker-text (error-message-string err)) nil ""))))
                 (header (concat " " (string-replace "%" "%%" (or title "")))))
      (unless (eq (window-buffer win) buf)
        (set-window-buffer win buf)
        (set-window-margins win 1))
      (unless (equal (window-parameter win 'header-line-format) header)
        (set-window-parameter win 'header-line-format header))
      (with-current-buffer buf
        (save-excursion
          (if (not pos)
              (progn
                (when (overlayp adh--picker-overlay)
                  (delete-overlay adh--picker-overlay))
                (set-window-start win (point-min))
                (set-window-point win (point-min)))
            (goto-char pos)
            (if (overlayp adh--picker-overlay)
                (move-overlay adh--picker-overlay (pos-bol) (pos-bol 2) buf)
              (setq adh--picker-overlay (make-overlay (pos-bol) (pos-bol 2)))
              (overlay-put adh--picker-overlay 'face 'adh-picker-preview-line)
              (overlay-put adh--picker-overlay 'priority 1000))
            (overlay-put adh--picker-overlay 'window win)
            (adh--picker-highlight-matches win matches)
            (forward-line (- (/ (window-body-height win) 3)))
            (set-window-start win (point))
            (set-window-point win pos))))
      (unless (frame-visible-p adh--picker-frame)
        (make-frame-visible adh--picker-frame)
        (unless (display-graphic-p)
          (run-with-timer 0.1 nil (lambda ()
                                    (when (window-live-p win)
                                      (force-window-update win))))))
      (setq minibuffer-scroll-window win))))

(defun adh--picker-update-when-idle ()
  "Update the preview of the active picker prompt."
  (setq adh--picker-idle-timer nil)
  (when-let* ((win (active-minibuffer-window)))
    (with-current-buffer (window-buffer win)
      (adh--picker-update))))

(defun adh--picker-update ()
  "Preview the current candidate if it, or what it matches, changed."
  (when adh--picker-active
    (if (and (not (eq adh--picker-shown 'none)) (input-pending-p))
        (unless (timerp adh--picker-idle-timer)
          (setq adh--picker-idle-timer
                (run-with-idle-timer 0 nil #'adh--picker-update-when-idle)))
      (let* ((target (ignore-errors (adh--picker-target)))
             (matches (ignore-errors (adh--picker-match-strings)))
             (shown (cons target matches)))
        (unless (equal shown adh--picker-shown)
          (let ((first (eq adh--picker-shown 'none)))
            (setq adh--picker-shown shown)
            (when (timerp adh--picker-timer)
              (cancel-timer adh--picker-timer))
            (if (or first (adh--picker-quick-p target))
                (adh--picker-render target matches)
              (setq adh--picker-timer
                    (run-with-timer adh-picker-preview-delay nil
                                    #'adh--picker-render target matches)))))))))

(defun adh--picker-set-commands (symbol value)
  "Set SYMBOL to VALUE and give the new commands the picker layout."
  (set-default-toplevel-value symbol value)
  (when (bound-and-true-p adh-picker-mode)
    (adh--picker-install)))

(defun adh--picker-locals (geo)
  "Return the minibuffer-local settings for the picker with layout GEO."
  `((vertico-buffer-display-action . (adh--picker-display-list))
    (vertico-buffer-hide-prompt . t)
    (mode-line-format . nil)
    (marginalia-field-width . ,(max 10 (/ (plist-get geo :list-cols) 3)))
    ,@(when (boundp 'marginalia-annotators)
        `((marginalia-annotators . ,(adh--picker-annotators))))))

(defun adh--picker-annotators ()
  "Return `marginalia-annotators' with `adh-picker-plain-categories' turned off."
  (mapcar (lambda (entry)
            (if (memq (car entry) adh-picker-plain-categories)
                (cons (car entry) (cons 'none (remq 'none (cdr entry))))
              entry))
          marginalia-annotators))

(defun adh--picker-workable-p ()
  "Non-nil if the selected frame can show child frames."
  (and (not noninteractive)
       (or (display-graphic-p) (featurep 'tty-child-frames))))

(defun adh--picker-default-modes ()
  "Return the modes of the default layout in `vertico-multiform-categories'."
  (cl-loop for setting in (cdr (assq t vertico-multiform-categories))
           for mode = (and (symbolp setting)
                           (let ((sym (intern-soft (format "vertico-%s-mode" setting))))
                             (if (and sym (fboundp sym)) sym setting)))
           when (and mode (fboundp mode) (boundp mode))
           collect mode))

(defun adh--picker-uninstall ()
  "Remove the picker from Vertico's per-command and per-category layouts."
  (cl-flet ((uninstall (layouts)
              (seq-remove (lambda (entry) (equal (cdr entry) '(adh--picker-session-mode)))
                          layouts)))
    (setq vertico-multiform-commands (uninstall vertico-multiform-commands)
          vertico-multiform-categories (uninstall vertico-multiform-categories))))

(defun adh--picker-install ()
  "Give the commands in `adh-picker-commands' the picker layout."
  (adh--picker-uninstall)
  (setq vertico-multiform-commands
        (append (mapcar (lambda (command) (list command 'adh--picker-session-mode))
                        adh-picker-commands)
                vertico-multiform-commands)))

(defun adh--apply-picker (on)
  "Turn the picker ON or off, for the option `adh-use-picker'."
  (adh-picker-mode (if on 1 -1)))

(defun adh-picker-define-preview (category function &optional quick)
  "Preview the candidates of completion CATEGORY with FUNCTION."
  (setf (alist-get category adh-picker-previews) (cons function quick)))

(defun adh-picker-preview-buffer (name text &optional mode)
  "Return a hidden buffer named after NAME that shows TEXT in major MODE."
  (with-current-buffer (get-buffer-create (format " *%s*" name) t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (delay-mode-hooks (funcall (or mode #'fundamental-mode)))
      (when mode
        (ignore-errors
          (font-lock-set-defaults)
          (jit-lock-register #'font-lock-fontify-region)))
      (setq-local truncate-lines t)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (current-buffer))))

(define-minor-mode adh--picker-session-mode
  "Show the current prompt in the floating picker."
  :global t
  (if adh--picker-session-mode
      (if (and (minibufferp)
               (adh--picker-workable-p)
               (require 'vertico-buffer nil t))
          (let ((geo (adh--picker-geometry)))
            (setq adh--picker-active t
                  adh--picker-shown 'none)
            (unless (display-graphic-p)
              (adh--picker-tty-borders))
            (adh--picker-setup-frame geo)
            (pcase-dolist (`(,var . ,value) (adh--picker-locals geo))
              (set (make-local-variable var) value))
            (unless vertico-buffer-mode
              (vertico-buffer-mode 1)
              (setq adh--picker-owns-buffer-mode t)))
        (dolist (mode (adh--picker-default-modes))
          (unless (symbol-value mode)
            (funcall mode 1)
            (push mode adh--picker-fallback-modes))))
    (dolist (mode adh--picker-fallback-modes)
      (funcall mode -1))
    (setq adh--picker-fallback-modes nil)
    (when adh--picker-active
      (setq adh--picker-active nil)
      (when adh--picker-owns-buffer-mode
        (setq adh--picker-owns-buffer-mode nil)
        (vertico-buffer-mode -1))
      (adh--picker-hide)
      (when (minibufferp)
        (mapc #'kill-local-variable adh--picker-local-vars)))))

(define-minor-mode adh-picker-mode
  "Open the prompts of `adh-picker-commands' in a floating picker."
  :global t
  :group 'adh-picker
  (if adh-picker-mode
      (progn
        (require 'vertico-multiform)
        (unless vertico-multiform-mode
          (vertico-multiform-mode 1))
        (add-to-list 'vertico-multiform--display-modes 'adh--picker-session-mode)
        (adh--picker-install)
        (add-function :after after-focus-change-function #'adh--picker-redirect-focus)
        (with-eval-after-load 'consult-imenu
          (advice-add 'consult-imenu--select :before #'adh--picker-remember-imenu)))
    (when (featurep 'vertico-multiform)
      (adh--picker-uninstall))
    (remove-function after-focus-change-function #'adh--picker-redirect-focus)
    (advice-remove 'consult-imenu--select #'adh--picker-remember-imenu)
    (adh--picker-flush-files)
    (when (frame-live-p adh--picker-frame)
      (delete-frame adh--picker-frame))))

(with-eval-after-load 'vertico
  (cl-defmethod vertico--display-candidates :after (_lines &context (adh--picker-session-mode (eql t)))
    "Update the picker preview after Vertico redisplays."
    (adh--picker-update)))

(adh--apply-picker adh-use-picker)

(provide 'adh-picker)

;;; adh-picker.el ends here
