;;; -*- lexical-binding: t; coding: utf-8 -*-

(defvar adh--ml-bg       "#181818" "Frame background; the mode line dissolves into it.")
(defvar adh--ml-isle     "#1f1f1f" "Island background.")
(defvar adh--ml-pill     "#282828" "Sub-island and pill background.")
(defvar adh--ml-fg       "#c8c8d5" "Primary text.")
(defvar adh--ml-muted    "#6b7570" "Secondary text.")
(defvar adh--ml-accent   "#96a6c8" "Identity: the file name and its project.")
(defvar adh--ml-faint    "#3e3b3c" "Dividers; quieter than what they divide.")
(defvar adh--ml-bg3      "#484848" "Palette bg3; only a mixing ingredient.")
(defvar adh--ml-inactive "#484848" "Present but not in force; the unmanaged LSP state.")
(defvar adh--ml-warn     "#f0ca54" "Needs care: unsaved changes, a root session.")
(defvar adh--ml-strong   "#333132" "Background of the box that marks a choice, a step above a grouping box.")

(defconst adh--ml-palette-map
  '((adh--ml-bg       . gruber-material-dark-bg0)
    (adh--ml-isle     . gruber-material-dark-bg0-5)
    (adh--ml-pill     . gruber-material-dark-bg1)
    (adh--ml-fg       . gruber-material-dark-fg0)
    (adh--ml-muted    . gruber-material-dark-quartz0)
    (adh--ml-accent   . gruber-material-dark-niagara2)
    (adh--ml-faint    . gruber-material-dark-bg2)
    (adh--ml-bg3      . gruber-material-dark-bg3)
    (adh--ml-warn     . gruber-material-dark-yellow))
  "Which palette entry each mode line colour is.")

(defun adh--ml-mix (a b)
  "Return the colour halfway between hex colours A and B."
  (if (and (stringp a) (stringp b)
           (string-match-p "\\`#[0-9a-fA-F]\\{6\\}\\'" a)
           (string-match-p "\\`#[0-9a-fA-F]\\{6\\}\\'" b))
      (apply #'format "#%02x%02x%02x"
             (mapcar (lambda (i)
                       (/ (+ (string-to-number (substring a i (+ i 2)) 16)
                             (string-to-number (substring b i (+ i 2)) 16))
                          2))
                     '(1 3 5)))
    a))

(defun adh--ml-sync-palette (&rest _)
  "Point the mode line colours at the active gruber palette."
  (let ((palette (or (and (boundp 'gruber-material-dark--palette-intense)
                          (symbol-value 'gruber-material-dark--palette-intense))
                     (and (boundp 'gruber-material-dark--palette)
                          (symbol-value 'gruber-material-dark--palette)))))
    (when palette
      (dolist (pair adh--ml-palette-map)
        (when-let* ((hex (cdr (assq (cdr pair) palette))))
          (set (car pair) hex)))))
  (setq adh--ml-strong (adh--ml-mix adh--ml-pill adh--ml-faint))
  (setq adh--ml-inactive (adh--ml-mix adh--ml-bg3 adh--ml-muted)))

(defconst adh--ml-cap-l "")
(defconst adh--ml-cap-r "")

(defconst adh--segment-modal-state-alist
  '((normal . ("N" . font-lock-variable-name-face))
    (insert . ("I" . font-lock-string-face))
    (motion . ("M" . font-lock-variable-name-face))))


(defun adh--ml-cap (glyph fg bg)
  "Render cap GLYPH in color FG against background BG."
  (propertize glyph 'face `(:foreground ,fg :background ,bg)))

(defun adh--ml-tint (part color)
  "Render PART in COLOR, keeping any faces it already carries."
  (let ((s (copy-sequence (if (stringp part) part (format-mode-line part)))))
    (unless (string-empty-p s)
      (add-face-text-property 0 (length s) `(:foreground ,color) t s))
    s))

(defun adh--ml-force (part color)
  "Render PART in COLOR, overriding any foreground it already carries."
  (let ((s (copy-sequence (if (stringp part) part (format-mode-line part)))))
    (unless (string-empty-p s)
      (add-face-text-property 0 (length s) `(:foreground ,color) nil s))
    s))

(defun adh--ml-escape (str)
  "Double every %% in STR so the mode line renders it literally."
  (replace-regexp-in-string "%" "%%" str t t))

(defun adh--ml-literal (construct)
  "Render CONSTRUCT for display, keeping any %% it contains literal."
  (let ((lit (cond ((stringp construct) construct)
                   ((and (symbolp construct) (boundp construct)
                         (stringp (symbol-value construct)))
                    (symbol-value construct)))))
    (adh--ml-escape (or lit (format-mode-line construct)))))

(defun adh--ml-dim (part)
  "Render PART in the secondary text color."
  (adh--ml-tint part adh--ml-muted))

(defun adh--ml-on-isle (part)
  "Give PART the island background, keeping any faces it already carries."
  (let ((s (copy-sequence (if (stringp part) part (format-mode-line part)))))
    (unless (string-empty-p s)
      (add-face-text-property 0 (length s) `(:background ,adh--ml-isle) t s))
    s))

(defun adh--ml-island (&rest parts)
  "Join PARTS, drop nils, and wrap the result in a rounded island."
  (let ((body (mapconcat #'adh--ml-on-isle (delq nil parts) "")))
    (unless (string-empty-p (string-trim body))
      (concat (adh--ml-cap adh--ml-cap-l adh--ml-isle adh--ml-bg)
              body
              (if (string-suffix-p adh--ml-cap-r body) "" (adh--ml-on-isle " "))
              (adh--ml-cap adh--ml-cap-r adh--ml-isle adh--ml-bg)))))

(defun adh--ml-pill (text fg)
  "Render TEXT as a bold pill in FG, capped against the island."
  (concat (adh--ml-cap adh--ml-cap-l adh--ml-pill adh--ml-isle)
          (propertize text 'face `(:foreground ,fg
                                   :background ,adh--ml-pill
                                   :weight bold))
          (adh--ml-cap adh--ml-cap-r adh--ml-pill adh--ml-isle)))

(defun adh--ml-box (bg parts)
  "Join non-nil PARTS and wrap them in a rounded box of background BG."
  (let ((body (mapconcat #'identity (delq nil parts) "  ")))
    (unless (string-empty-p body)
      (concat (adh--ml-cap adh--ml-cap-l bg adh--ml-isle)
              (let ((b (copy-sequence (concat " " body " "))))
                (add-face-text-property 0 (length b) `(:background ,bg) t b)
                b)
              (adh--ml-cap adh--ml-cap-r bg adh--ml-isle)))))

(defun adh--ml-sub (&rest parts)
  "Group PARTS in a quiet box, or nil when all are empty."
  (adh--ml-box adh--ml-pill parts))

(defun adh--ml-mark (&rest parts)
  "Box PARTS as the chosen one of several visible siblings."
  (adh--ml-box adh--ml-strong parts))

(defface adh-mode-line-buffer-id-inactive
  '((t (:inherit shadow)))
  "Face for the buffer name in mode lines of unselected windows."
  :group 'mode-line-faces)

(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))
(declare-function adh--get-project-dir "adh-project" (&optional dir))

(defvar-local adh--ml-project-root 'unset
  "This buffer's project root as a directory, nil for none, `unset' if unasked.")

(defun adh--ml-note-project (&optional allow-remote)
  "Resolve and cache this buffer's project root."
  (setq adh--ml-project-root
        (and default-directory
             (or allow-remote (not (file-remote-p default-directory)))
             (cond ((fboundp 'adh--get-project-dir)
                    (adh--get-project-dir))
                   ((fboundp 'project-current)
                    (when-let* ((proj (project-current nil)))
                      (project-root proj)))))))

(defun adh--ml-note-project-eagerly ()
  "Resolve the project root on `find-file-hook', remote included."
  (adh--ml-note-project t))

(add-hook 'find-file-hook #'adh--ml-note-project-eagerly)

(defun adh--ml-visit-project-root (event)
  "Open the project root of the buffer whose mode line was clicked."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (when (stringp adh--ml-project-root)
      (find-file adh--ml-project-root))))

(defvar adh--ml-project-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line mouse-1] #'adh--ml-visit-project-root)
    m)
  "Keymap on the project name.")

(defun adh--segment-project ()
  "Return the project's directory name, clickable to jump to its root."
  (when (eq adh--ml-project-root 'unset)
    (adh--ml-note-project))
  (when (stringp adh--ml-project-root)
    (propertize (adh--ml-escape
                 (file-name-nondirectory
                  (directory-file-name adh--ml-project-root)))
                'face `(:foreground ,adh--ml-accent)
                'mouse-face 'mode-line-highlight
                'help-echo (concat adh--ml-project-root
                                   "\nmouse-1: open project root")
                'local-map adh--ml-project-map)))

(defun adh--segment-file ()
  "Return the buffer name, carrying whether it can be, or has been, edited."
  (propertize
   (adh--ml-escape (buffer-name))
   'face (if (mode-line-window-selected-p)
             `(:foreground ,(if (and buffer-file-name (buffer-modified-p))
                                adh--ml-warn
                              adh--ml-accent)
               :weight bold
               :slant ,(if buffer-read-only 'italic 'normal))
           'adh-mode-line-buffer-id-inactive)
   'mouse-face 'mode-line-highlight
   'help-echo (concat "Buffer name"
                      (cond (buffer-read-only " (read-only)")
                            ((buffer-modified-p) " (modified)")
                            (t ""))
                      "\nmouse-1: Previous buffer\nmouse-3: Next buffer")
   'local-map mode-line-buffer-identification-keymap))

(defun adh--ml-set-coding-system (event)
  "Prompt for this buffer's coding system, as \\[set-buffer-file-coding-system]."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (call-interactively #'set-buffer-file-coding-system)))

(defvar adh--ml-encoding-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line mouse-1] #'adh--ml-set-coding-system)
    m)
  "Keymap on the encoding segment.")

(defun adh--segment-eol ()
  "Return the line-ending style, but only when it is not unix."
  (let ((eol (coding-system-eol-type buffer-file-coding-system)))
    (when (memq eol '(1 2))
      (propertize (if (eq eol 1) "crlf" "cr")
                  'face `(:foreground ,adh--ml-fg)))))

(defun adh--segment-coding ()
  "Return the buffer's coding system, or nil when it has none."
  (when buffer-file-coding-system
    (let* ((sys (coding-system-plist buffer-file-coding-system))
           (cat (plist-get sys :category))
           (sym (if (memq cat '(coding-category-undecided coding-category-utf-8))
                    'utf-8
                  (plist-get sys :name))))
      (propertize (adh--ml-escape (downcase (symbol-name sym)))
                  'face `(:foreground ,adh--ml-fg)))))

(defun adh--segment-encoding ()
  "Return the coding system, with the line ending after a slash when unusual."
  (let* ((coding (adh--segment-coding))
         (eol (adh--segment-eol))
         (body (cond ((and coding eol)
                      (concat coding
                              (propertize "/" 'face `(:foreground ,adh--ml-fg))
                              eol))
                     (coding)
                     (eol))))
    (when body
      (let ((s (copy-sequence body)))
        (add-text-properties
         0 (length s)
         (list 'mouse-face 'mode-line-highlight
               'help-echo (format "Coding system: %s\nmouse-1: set coding system"
                                  (or buffer-file-coding-system "none"))
               'local-map adh--ml-encoding-map)
         s)
        s))))

(defun adh--segment-remote ()
  "Return the remote indicator, or nil for a local file."
  (when-let* ((host (and default-directory
                         (file-remote-p default-directory 'host))))
    (let* ((host (substring-no-properties host))
           (method (let ((m (file-remote-p default-directory 'method)))
                     (and m (substring-no-properties m))))
           (user (let ((u (file-remote-p default-directory 'user)))
                   (and u (substring-no-properties u))))
           (root (equal user "root"))
           (label (adh--ml-escape
                   (if (equal method "ssh")
                       (concat "@" host)
                     (concat method ":" host)))))
      (propertize label
                  'face `(:foreground ,(if root adh--ml-warn adh--ml-fg))
                  'mouse-face 'mode-line-highlight
                  'help-echo (format "Remote file\nmethod: %s\nhost: %s\nuser: %s"
                                     (or method "?") host (or user "(default)"))))))

(defun adh--segment-position ()
  "Return row:col and how far the cursor is through the buffer, as a percentage."
  (let* ((rowcol (format-mode-line '((line-number-mode "%l")
                                     (column-number-mode ":%c"))))
         (span (- (point-max) (point-min)))
         (pct (if (zerop span)
                  0
                (/ (* 100 (- (point) (point-min))) span))))
    (adh--ml-tint (adh--ml-escape (concat rowcol " " (format "%d%%" pct)))
                  adh--ml-fg)))

(defun adh--segment-modal-fn ()
  "Return the current meow state as a faced N/I/M pill, or nil."
  (when (boundp 'meow--current-state)
    (let* ((mode-cons (alist-get meow--current-state
                                 adh--segment-modal-state-alist))
           (label (car-safe mode-cons))
           (face (cdr-safe mode-cons)))
      (when label
        (propertize label
                    'face `(:foreground ,(or (face-foreground face nil t)
                                             adh--ml-fg)
                            :weight bold))))))

(defun adh--segment-major-mode ()
  "Return the major mode and its process."
  (let* ((name (string-trim (adh--ml-literal mode-name)))
         (proc (string-trim (adh--ml-literal mode-line-process)))
         (text (if (string-empty-p proc) name (concat name " " proc))))
    (unless (string-empty-p text)
      (concat (format-mode-line "%[")
              (adh--ml-tint text adh--ml-fg)
              (let ((n (format-mode-line "%n")))
                (if (string-empty-p n) "" (adh--ml-dim n)))
              (format-mode-line "%]")))))

(defun adh--segment-tab ()
  "Return a tab list when more than one tab exists, each clickable."
  (let ((tabs (tab-bar-tabs)))
    (when (> (length tabs) 1)
      (let ((current-name (alist-get 'name (tab-bar--current-tab))))
        (mapconcat
         #'identity
         (seq-map-indexed
          (lambda (tab idx)
            (let* ((raw (alist-get 'name tab))
                   (name (adh--ml-escape raw))
                   (n (1+ idx))
                   (body (if (string= raw current-name)
                             (adh--ml-mark
                              (propertize name
                                          'face `(:foreground ,adh--ml-fg)))
                           (propertize name
                                       'face `(:foreground ,adh--ml-muted))))
                   (map (let ((m (make-sparse-keymap)))
                          (define-key m [mode-line mouse-1]
                                      (lambda (e)
                                        (interactive "e")
                                        (with-selected-window
                                            (posn-window (event-start e))
                                          (tab-bar-select-tab n))))
                          m))
                   (s (copy-sequence body)))
              (add-text-properties
               0 (length s)
               (list 'mouse-face 'mode-line-highlight
                     'help-echo (format "Tab %d: %s\nmouse-1: switch to this tab"
                                        n raw)
                     'local-map map)
               s)
              s))
          tabs)
         " ")))))

(defvar adh--ml-lsp-epoch 0
  "Bumped whenever eglot's set of servers may have changed.")

(defun adh--ml-bump-lsp-epoch (&rest _)
  "Invalidate every buffer's cached LSP state."
  (setq adh--ml-lsp-epoch (1+ adh--ml-lsp-epoch)))

(with-eval-after-load 'eglot
  (add-hook 'eglot-server-initialized-hook #'adh--ml-bump-lsp-epoch)
  (add-hook 'eglot-managed-mode-hook #'adh--ml-bump-lsp-epoch))

(defvar-local adh--ml-lsp-cache nil
  "Cons of (EPOCH . STATE) for this buffer.")

(defun adh--ml-lsp-server-covers-p ()
  "Return non-nil when some running eglot server's project contains this file."
  (and (boundp 'eglot--servers-by-project)
       default-directory
       (not (file-remote-p default-directory))
       (let ((here (expand-file-name default-directory))
             (found nil))
         (maphash
          (lambda (proj servers)
            (unless found
              (when-let* ((_ servers)
                          (root (ignore-errors (project-root proj))))
                (when (file-in-directory-p here root)
                  (setq found t)))))
          (symbol-value 'eglot--servers-by-project))
         found)))

(defun adh--ml-lsp-state ()
  "Return `managed', `project', or nil."
  (unless (and adh--ml-lsp-cache (eq (car adh--ml-lsp-cache) adh--ml-lsp-epoch))
    (setq adh--ml-lsp-cache
          (cons adh--ml-lsp-epoch
                (cond
                 ((and (fboundp 'eglot-managed-p) (eglot-managed-p)) 'managed)
                 ((adh--ml-lsp-server-covers-p) 'project)))))
  (cdr adh--ml-lsp-cache))

(defun adh--ml-lsp-button (text color)
  "Render TEXT in COLOR carrying eglot's own mode line menu."
  (let ((s (adh--ml-force text color)))
    (when (boundp 'eglot--main-menu-map)
      (add-text-properties
       0 (length s)
       (list 'mouse-face 'mode-line-highlight
             'help-echo "Eglot: Emacs LSP client\nmouse-1: Display minor mode menu"
             'keymap (symbol-value 'eglot--main-menu-map))
       s))
    s))

(defun adh--segment-lsp ()
  "Return the LSP indicator: white when this buffer is managed, dim otherwise."
  (pcase (adh--ml-lsp-state)
    ('managed
     (let ((extra (and (boundp 'eglot-mode-line-format)
                       (delete "" (mapcar #'format-mode-line
                                          (seq-difference
                                           eglot-mode-line-format
                                           '(eglot-mode-line-menu
                                             eglot-mode-line-session)))))))
       (concat (adh--ml-lsp-button "lsp" adh--ml-fg)
               (when extra
                 (let ((txt (adh--ml-escape
                             (string-trim (mapconcat #'identity extra " ")))))
                   (unless (string-empty-p txt)
                     (concat " " (adh--ml-force txt adh--ml-fg))))))))
    ('project (adh--ml-lsp-button "lsp" adh--ml-inactive))))

(defconst adh--ml-minors-excluded
  '(flymake-mode eglot--managed-mode
    meow-normal-mode meow-insert-mode meow-motion-mode
    company-posframe-mode)
  "Minor modes that have a segment of their own, or none worth showing.")

(defun adh--ml-active-minor-modes ()
  "Return the lighters of enabled minor modes, trimmed, empties dropped."
  (let (out)
    (dolist (entry minor-mode-alist (nreverse out))
      (let ((sym (car entry)))
        (when (and (not (memq sym adh--ml-minors-excluded))
                   (boundp sym) (symbol-value sym))
          (let* ((tail (cdr entry))
                 (lighter (if (consp tail) (car tail) tail))
                 (s (string-trim (adh--ml-literal lighter))))
            (unless (string-empty-p s)
              (push s out))))))))

(defvar-local adh--ml-minors-expanded nil
  "When non-nil, list the minor mode lighters instead of counting them.")

(defun adh-toggle-minor-modes ()
  "Expand or contract this buffer's minor mode list in the mode line."
  (interactive)
  (setq adh--ml-minors-expanded (not adh--ml-minors-expanded))
  (force-mode-line-update))

(defun adh--ml-click-minor-modes (event)
  "Toggle the minor mode list of the buffer whose mode line was clicked."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (adh-toggle-minor-modes)))

(defvar adh--ml-minors-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line mouse-1] #'adh--ml-click-minor-modes)
    m)
  "Keymap on the minor mode segment.")

(defun adh--segment-minor-modes ()
  "Return the minor modes: a count when contracted, the list when expanded."
  (let ((modes (adh--ml-active-minor-modes)))
    (when modes
      (let ((s (if adh--ml-minors-expanded
                   (mapconcat (lambda (m)
                                (propertize (downcase m)
                                            'face `(:foreground ,adh--ml-fg)))
                              modes
                              " ")
                 (propertize (format "⋯%d" (length modes))
                             'face `(:foreground ,adh--ml-muted)))))
        (add-text-properties
         0 (length s)
         (list 'mouse-face 'mode-line-highlight
               'help-echo (concat (number-to-string (length modes))
                                  " minor modes:\n  "
                                  (mapconcat #'identity modes "\n  ")
                                  "\n\nmouse-1: "
                                  (if adh--ml-minors-expanded "contract" "expand"))
               'local-map adh--ml-minors-map)
         s)
        s))))

(defun adh--segment-flymake ()
  "Return flymake's non-zero counters, or a quiet check when the buffer is clean."
  (when (and (bound-and-true-p flymake-mode)
             (boundp 'flymake-mode-line-format))
    (let* ((s (format-mode-line (symbol-value 'flymake-mode-line-format)))
           (n (length s))
           (i 0)
           all)
      (while (< i n)
        (let ((next (or (next-single-property-change i 'flymake--diagnostic-type s) n)))
          (when (get-text-property i 'flymake--diagnostic-type s)
            (push (substring s i next) all))
          (setq i next)))
      (setq all (nreverse all))
      (when all
        (let ((nonzero (seq-remove
                        (lambda (c)
                          (equal (string-trim (substring-no-properties c)) "0"))
                        all)))
          (if nonzero
              (mapconcat #'identity nonzero
                         (propertize "·" 'face `(:foreground ,adh--ml-faint)))
            (let ((tick (apply #'propertize "✓"
                               (text-properties-at 0 (car all)))))
              (add-face-text-property 0 1 `(:foreground ,adh--ml-muted) nil tick)
              (put-text-property 0 1 'help-echo "No diagnostics.\nmouse-1: list" tick)
              tick)))))))

(defun adh--segment-tooling ()
  "LSP state and flymake's verdict as one reading, or nil when neither runs."
  (let ((lsp (adh--segment-lsp))
        (fly (adh--segment-flymake)))
    (cond ((and lsp fly)
           (concat lsp
                   (propertize ":" 'face `(:foreground ,adh--ml-fg))
                   fly))
          (lsp)
          (fly))))

(defun adh--segment-modes ()
  "Major mode and the active minor modes, grouped in one sub-island."
  (adh--ml-sub (adh--segment-major-mode) (adh--segment-minor-modes)))

(defun adh--ml-right ()
  "Build the right island: where the buffer came from and what the tooling says."
  (when (mode-line-window-selected-p)
    (let ((parts (delq nil (list (adh--segment-tooling)
                                 (adh--segment-remote)
                                 (adh--segment-encoding)
                                 (adh--segment-project)))))
      (when parts
        (adh--ml-island " " (mapconcat #'identity parts "  "))))))

(defun adh--ml-left ()
  "Build the left island: what you are doing."
  (if (mode-line-window-selected-p)
      (adh--ml-island
       " "
       (when-let* ((m (adh--segment-modal-fn))) (concat m "  "))
       (adh--ml-sub (adh--segment-file) (adh--segment-position))
       (when-let* ((modes (adh--segment-modes))) (concat "  " modes))
       (when-let* ((tabs (adh--segment-tab))) (concat "  " tabs)))
    (adh--ml-island " " (adh--segment-file))))

(defun adh--ml-quiet-p ()
  "Return non-nil while the minibuffer is taking input."
  (and (active-minibuffer-window) t))

(defun adh--ml-width (part)
  "Return the columns PART occupies once the mode line collapses its escapes."
  (string-width (format-mode-line part)))

(defun adh--ml-compose ()
  "Assemble the mode line: main island, transparent gap, right island if any."
  (let ((left  (adh--ml-left))
        (right (adh--ml-right)))
    (if (not right)
        left
      (concat left
              (propertize " " 'display
                          `(space :align-to
                                  (- (+ right right-fringe right-margin)
                                     ,(adh--ml-width right))))
              right))))

(defun adh--ml-render ()
  "Render the mode line, quietened while the minibuffer is active."
  (if (adh--ml-quiet-p)
      (let ((adh--ml-accent adh--ml-muted)
            (adh--ml-fg     adh--ml-muted)
            (adh--ml-warn   adh--ml-muted)
            (adh--ml-pill   adh--ml-isle)
            (adh--ml-strong adh--ml-isle))
        (adh--ml-compose))
    (adh--ml-compose)))

(defun adh--ml-minibuffer-refresh ()
  "Force every mode line to redraw, for `adh--ml-quiet-p'."
  (force-mode-line-update t))

(add-hook 'minibuffer-setup-hook #'adh--ml-minibuffer-refresh)
(add-hook 'minibuffer-exit-hook  #'adh--ml-minibuffer-refresh)

(setq-default mode-line-format '("%e" (:eval (adh--ml-render))))

(defun adh--ml-flatten-faces (&rest _)
  "Make the mode line background match the frame and drop its border."
  (dolist (face '(mode-line mode-line-active mode-line-inactive))
    (when (facep face)
      (set-face-attribute face nil
                          :box nil
                          :overline nil
                          :underline nil
                          :background adh--ml-bg
                          :foreground adh--ml-muted))))

(defun adh--ml-refresh-theme (&rest _)
  "Re-read the palette, then re-flatten the mode line faces."
  (adh--ml-sync-palette)
  (adh--ml-flatten-faces))

(adh--ml-refresh-theme)
(add-hook 'enable-theme-functions #'adh--ml-refresh-theme)

(provide 'adh-modeline)
