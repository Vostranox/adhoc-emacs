;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-functions)

(defvar adh--ml-bg       "#181818")
(defvar adh--ml-isle     "#1f1f1f")
(defvar adh--ml-pill     "#282828")
(defvar adh--ml-fg       "#c8c8d5")
(defvar adh--ml-muted    "#6b7570")
(defvar adh--ml-accent   "#96a6c8")
(defvar adh--ml-faint    "#3e3b3c")
(defvar adh--ml-bg3      "#484848")
(defvar adh--ml-inactive "#484848")
(defvar adh--ml-warn     "#f0ca54")
(defvar adh--ml-strong   "#333132")

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

(defun adh--ml-sync-palette ()
  "Point the mode line colours at the active gruber palette."
  (when-let* ((palette (if (eq (car custom-enabled-themes) 'gruber-material-dark)
                           (bound-and-true-p gruber-material-dark--palette)
                         (bound-and-true-p gruber-material-dark--palette-intense))))
    (dolist (pair adh--ml-palette-map)
      (when-let* ((hex (cdr (assq (cdr pair) palette))))
        (set (car pair) hex))))
  (setq adh--ml-strong (adh--ml-mix adh--ml-pill adh--ml-faint))
  (setq adh--ml-inactive (adh--ml-mix adh--ml-bg3 adh--ml-muted)))

(defconst adh--ml-cap-l "")
(defconst adh--ml-cap-r "")

(defconst adh--ml-modal-states
  '((normal "N" . font-lock-function-name-face)
    (insert "I" . warning)
    (motion "M" . homoglyph)))

(defun adh--ml-translucent-p ()
  "Return non-nil if this frame's background is see-through."
  (let ((alpha (frame-parameter nil 'alpha-background)))
    (and (numberp alpha) (< alpha (if (floatp alpha) 1.0 100)))))

(defun adh--ml-cap (glyph fg bg)
  "Render cap GLYPH in color FG against background BG."
  (if (adh--ml-translucent-p)
      (propertize " " 'face `(:background ,fg) 'adh--ml-cap t)
    (propertize glyph 'face `(:foreground ,fg :background ,bg) 'adh--ml-cap t)))

(defun adh--ml-tint (part color &optional override)
  "Render PART in COLOR, under the faces it carries unless OVERRIDE."
  (let ((s (copy-sequence part)))
    (add-face-text-property 0 (length s) `(:foreground ,color) (not override) s)
    s))

(defun adh--ml-escape (str)
  "Double every %% in STR so the mode line renders it literally."
  (string-replace "%" "%%" str))

(defun adh--ml-literal (construct)
  "Render CONSTRUCT for display, keeping any %% it contains literal."
  (adh--ml-escape (if (stringp construct) construct (format-mode-line construct))))

(defun adh--ml-island (&rest parts)
  "Join PARTS, drop nils, and wrap the result in a rounded island."
  (let ((body (apply #'concat parts)))
    (when (string-match-p "[^ \t\n\r]" body)
      (unless (get-text-property (1- (length body)) 'adh--ml-cap body)
        (setq body (concat body " ")))
      (add-face-text-property 0 (length body) `(:background ,adh--ml-isle) t body)
      (concat (adh--ml-cap adh--ml-cap-l adh--ml-isle adh--ml-bg)
              body
              (adh--ml-cap adh--ml-cap-r adh--ml-isle adh--ml-bg)))))

(defun adh--ml-box (bg parts)
  "Join non-nil PARTS and wrap them in a rounded box of background BG."
  (let ((body (mapconcat #'identity (delq nil parts) "  ")))
    (unless (string-empty-p body)
      (concat (adh--ml-cap adh--ml-cap-l bg adh--ml-isle)
              (let ((b (concat " " body " ")))
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

(declare-function project-root "project" (project))

(defvar-local adh--ml-project-roots nil
  "Alist of the directories looked up in this buffer and their project roots.")

(defun adh--ml-note-project ()
  "Resolve and cache the project root of `default-directory'."
  (setf (alist-get default-directory adh--ml-project-roots nil nil #'equal)
        (adh--get-project-dir)))

(defun adh--ml-project-root ()
  "Return the project root of `default-directory', resolving it when local."
  (if-let* ((hit (assoc default-directory adh--ml-project-roots)))
      (cdr hit)
    (and default-directory
         (not (file-remote-p default-directory))
         (adh--ml-note-project))))

(add-hook 'find-file-hook #'adh--ml-note-project)

(defun adh--ml-visit-project-root (event)
  "Open the project root of the buffer whose mode line was clicked."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (when-let* ((root (adh--ml-project-root)))
      (find-file root))))

(defvar adh--ml-project-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line mouse-1] #'adh--ml-visit-project-root)
    m)
  "Keymap on the project name.")

(defun adh--segment-project ()
  "Return the project's directory name, clickable to jump to its root."
  (when-let* ((root (adh--ml-project-root)))
    (propertize (adh--ml-escape
                 (file-name-nondirectory (directory-file-name root)))
                'face `(:foreground ,adh--ml-accent)
                'mouse-face 'mode-line-highlight
                'help-echo (concat root "\nmouse-1: open project root")
                'local-map adh--ml-project-map)))

(defun adh--ml-dired-p ()
  "Return non-nil in a Dired buffer that lists a directory."
  (and (derived-mode-p '(dired-mode wdired-mode)) (not (bound-and-true-p dirvish-fd-buffer))))

(defun adh--ml-dired-name ()
  "Return the name of the directory a Dired buffer lists, ending in a slash."
  (let* ((dir (abbreviate-file-name (file-local-name default-directory)))
         (name (file-name-nondirectory (directory-file-name dir))))
    (if (equal name "") dir (file-name-as-directory name))))

(defun adh--segment-file ()
  "Return the buffer name, carrying whether it can be, or has been, edited.
A Dired buffer shows the name of its directory instead."
  (let ((active (mode-line-window-selected-p))
        (dired (adh--ml-dired-p)))
    (propertize
     (concat
      (propertize (adh--ml-escape (if dired (adh--ml-dired-name) (buffer-name)))
                  'face (if active
                            `(:foreground ,adh--ml-accent
                              :weight bold
                              :slant ,(if buffer-read-only 'italic 'normal))
                          'adh-mode-line-buffer-id-inactive))
      (when buffer-file-name
        (let ((face (list '(:height 0.8)
                          (if active
                              `(:foreground ,adh--ml-warn)
                            'adh-mode-line-buffer-id-inactive))))
          (if (buffer-modified-p)
              (propertize "•" 'face face 'display '(raise 0.15))
            (propertize " " 'face face)))))
     'mouse-face 'mode-line-highlight
     'help-echo (concat (if dired (abbreviate-file-name default-directory) "Buffer name")
                        (cond (buffer-read-only " (read-only)")
                              ((buffer-modified-p) " (modified)")
                              (t ""))
                        "\nmouse-1: Previous buffer\nmouse-3: Next buffer")
     'local-map mode-line-buffer-identification-keymap)))

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

(defun adh--segment-encoding ()
  "Return the coding system, plus the line ending when not unix."
  (when buffer-file-coding-system
    (let ((sys (coding-system-plist buffer-file-coding-system)))
      (propertize (concat (if (memq (plist-get sys :category)
                                    '(coding-category-undecided coding-category-utf-8))
                              "utf-8"
                            (symbol-name (plist-get sys :name)))
                          (pcase (coding-system-eol-type buffer-file-coding-system)
                            (1 "/crlf")
                            (2 "/cr")))
                  'face `(:foreground ,adh--ml-fg)
                  'mouse-face 'mode-line-highlight
                  'help-echo (format "Coding system: %s\nmouse-1: set coding system"
                                     buffer-file-coding-system)
                  'local-map adh--ml-encoding-map))))

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
                  'face `(:foreground ,(if root adh--ml-warn adh--ml-muted))
                  'mouse-face 'mode-line-highlight
                  'help-echo (format "Remote file\nmethod: %s\nhost: %s\nuser: %s"
                                     (or method "?") host (or user "(default)"))))))

(defun adh--segment-position ()
  "Return row:col and the position through the buffer in percent."
  (let* ((rowcol (format-mode-line '((line-number-mode "%l")
                                     (column-number-mode ":%c"))))
         (span (- (point-max) (point-min)))
         (pct (if (zerop span)
                  0
                (/ (* 100 (- (point) (point-min))) span))))
    (adh--ml-tint (concat rowcol " " (number-to-string pct) "%%") adh--ml-fg)))

(defun adh--segment-modal ()
  "Return the current meow state as a bold N/I/M letter, or nil."
  (when-let* ((entry (alist-get (bound-and-true-p meow--current-state)
                                adh--ml-modal-states)))
    (propertize (car entry)
                'face `(:foreground ,(or (face-foreground (cdr entry) nil t)
                                         adh--ml-fg)
                        :weight bold))))

(defun adh--segment-major-mode ()
  "Return the major mode and its process."
  (let* ((name (string-trim (adh--ml-literal mode-name)))
         (proc (string-trim (adh--ml-literal mode-line-process)))
         (text (if (string-empty-p proc) name (concat name " " proc))))
    (unless (string-empty-p text)
      (concat "%[" (adh--ml-tint text adh--ml-fg) (adh--ml-tint "%n" adh--ml-muted) "%]"))))

(defun adh--ml-select-tab (event)
  "Switch to the tab whose name was clicked."
  (interactive "e")
  (when-let* ((obj (posn-string (event-start event)))
              (n (get-text-property (cdr obj) 'adh--ml-tab (car obj))))
    (with-selected-window (posn-window (event-start event))
      (tab-bar-select-tab n))))

(defvar adh--ml-tab-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line mouse-1] #'adh--ml-select-tab)
    m)
  "Keymap on each tab name.")

(defun adh--segment-tab ()
  "Return a tab list when more than one tab exists, each clickable."
  (let ((tabs (tab-bar-tabs)))
    (when (> (length tabs) 1)
      (mapconcat
       #'identity
       (seq-map-indexed
        (lambda (tab idx)
          (let* ((raw (alist-get 'name tab))
                 (name (adh--ml-escape raw))
                 (n (1+ idx)))
            (propertize (if (eq (car tab) 'current-tab)
                            (adh--ml-mark
                             (propertize name 'face `(:foreground ,adh--ml-fg)))
                          (propertize name 'face `(:foreground ,adh--ml-muted)))
                        'mouse-face 'mode-line-highlight
                        'help-echo (format "Tab %d: %s\nmouse-1: switch to this tab"
                                           n raw)
                        'adh--ml-tab n
                        'local-map adh--ml-tab-map)))
        tabs)
       " "))))

(defvar adh--ml-lsp-epoch 0
  "Bumped whenever eglot's set of servers may have changed.")

(defun adh--ml-bump-lsp-epoch (&rest _)
  "Invalidate every buffer's cached LSP state."
  (setq adh--ml-lsp-epoch (1+ adh--ml-lsp-epoch)))

(with-eval-after-load 'eglot
  (add-hook 'eglot-connect-hook #'adh--ml-bump-lsp-epoch)
  (add-hook 'eglot-managed-mode-hook #'adh--ml-bump-lsp-epoch))

(defun adh--ml-lsp-toggled (&rest _)
  "Redraw every mode line's LSP state after global eglot is switched."
  (adh--ml-bump-lsp-epoch)
  (force-mode-line-update t))

(add-variable-watcher 'adh--eglot-global-enabled #'adh--ml-lsp-toggled)

(defvar-local adh--ml-lsp-cache nil
  "Cons of (EPOCH . STATE) for this buffer.")

(defun adh--ml-lsp-server-covers-p ()
  "Return non-nil if a running eglot server's project contains this file."
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
  "Return `managed', `unmanaged', or nil."
  (unless (and adh--ml-lsp-cache (eq (car adh--ml-lsp-cache) adh--ml-lsp-epoch))
    (setq adh--ml-lsp-cache
          (cons adh--ml-lsp-epoch
                (cond
                 ((and (fboundp 'eglot-managed-p) (eglot-managed-p)) 'managed)
                 ((or (bound-and-true-p adh--eglot-global-enabled)
                      (adh--ml-lsp-server-covers-p))
                  'unmanaged)))))
  (cdr adh--ml-lsp-cache))

(defun adh--ml-lsp-button (color)
  "Render \"lsp\" in COLOR carrying eglot's own mode line menu."
  (propertize "lsp"
              'face `(:foreground ,color)
              'mouse-face 'mode-line-highlight
              'help-echo "Eglot: Emacs LSP client\nmouse-1: Display minor mode menu"
              'keymap (symbol-value 'eglot--main-menu-map)))

(defun adh--segment-lsp ()
  "Return the LSP indicator, bright when this buffer is managed."
  (pcase (adh--ml-lsp-state)
    ('managed
     (let ((extra (adh--ml-escape
                   (string-trim
                    (mapconcat #'identity
                               (delete "" (mapcar #'format-mode-line
                                                  (seq-difference
                                                   (symbol-value 'eglot-mode-line-format)
                                                   '(eglot-mode-line-menu
                                                     eglot-mode-line-session))))
                               " ")))))
       (concat (adh--ml-lsp-button adh--ml-fg)
               (unless (string-empty-p extra)
                 (concat " " (adh--ml-tint extra adh--ml-fg t))))))
    ('unmanaged (adh--ml-lsp-button adh--ml-inactive))))

(defconst adh--ml-minors-excluded
  '(flymake-mode
    meow-normal-mode meow-insert-mode meow-motion-mode
    completion-preview-mode)
  "Minor modes that have a segment of their own, or none worth showing.")

(defun adh--ml-active-minor-modes ()
  "Return (MODE . LIGHTER) for the enabled minor modes that show a lighter."
  (let (out)
    (dolist (entry minor-mode-alist (nreverse out))
      (let ((sym (car entry)))
        (when (and (not (memq sym adh--ml-minors-excluded))
                   (boundp sym) (symbol-value sym))
          (let ((s (string-trim (adh--ml-literal (car-safe (cdr entry))))))
            (unless (string-empty-p s)
              (push (cons sym s) out))))))))

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
  "Keymap on the minor mode toggle.")

(defun adh--ml-minor-mode-at (event)
  "Return the minor mode whose lighter EVENT is on, or nil."
  (when-let* ((obj (posn-string (event-start event))))
    (get-text-property (cdr obj) 'adh--ml-minor-mode (car obj))))

(defun adh--ml-minor-mode-menu (event)
  "Show the menu of the minor mode whose lighter was clicked."
  (interactive "@e")
  (when-let* ((mode (adh--ml-minor-mode-at event)))
    (minor-mode-menu-from-indicator mode (posn-window (event-start event)) event)))

(defun adh--ml-minor-mode-help (event)
  "Describe the minor mode whose lighter was clicked."
  (interactive "@e")
  (when-let* ((mode (adh--ml-minor-mode-at event)))
    (describe-minor-mode-from-symbol (or (get mode :minor-mode-function) mode))))

(defvar adh--ml-minor-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line down-mouse-1] #'adh--ml-minor-mode-menu)
    (define-key m [mode-line mouse-2] #'adh--ml-minor-mode-help)
    m)
  "Keymap on each minor mode lighter.")

(defun adh--segment-minor-modes ()
  "Return the minor mode count, or the lighters when expanded.
The count toggles the list; a lighter opens its mode's menu."
  (let ((modes (adh--ml-active-minor-modes)))
    (when modes
      (let ((toggle (propertize
                     (if adh--ml-minors-expanded "⋯" (format "⋯%d" (length modes)))
                     'face `(:foreground ,adh--ml-muted)
                     'mouse-face 'mode-line-highlight
                     'help-echo (if adh--ml-minors-expanded "mouse-1: contract" "mouse-1: expand")
                     'local-map adh--ml-minors-map)))
        (if (not adh--ml-minors-expanded)
            toggle
          (concat toggle " "
                  (mapconcat
                   (lambda (mode)
                     (propertize (downcase (cdr mode))
                                 'face `(:foreground ,adh--ml-fg)
                                 'mouse-face 'mode-line-highlight
                                 'help-echo (format "%s\nmouse-1: Display minor mode menu\nmouse-2: Show help for minor mode"
                                                    (car mode))
                                 'adh--ml-minor-mode (car mode)
                                 'local-map adh--ml-minor-mode-map))
                   modes " ")))))))

(defun adh--segment-flymake ()
  "Return flymake's non-zero counters, or a check mark when clean."
  (when (bound-and-true-p flymake-mode)
    (let* ((s (format-mode-line 'flymake-mode-line-counters))
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
        (let ((nonzero (remove "0" all)))
          (if nonzero
              (mapconcat #'identity nonzero
                         (propertize "·" 'face `(:foreground ,adh--ml-faint)))
            (let ((tick (apply #'propertize "✓"
                               (text-properties-at 0 (car all)))))
              (add-face-text-property 0 1 `(:foreground ,adh--ml-muted) nil tick)
              (put-text-property 0 1 'help-echo "No diagnostics.\nmouse-1: list" tick)
              tick)))))))

(defun adh--segment-tooling ()
  "Return the LSP state and flymake counters together, or nil."
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
  "Build the right island: tooling, remote, encoding and project."
  (when (mode-line-window-selected-p)
    (let ((parts (delq nil (list (adh--segment-tooling)
                                 (adh--segment-remote)
                                 (adh--segment-encoding)
                                 (adh--segment-project)))))
      (adh--ml-island " " (mapconcat #'identity parts "  ")))))

(defun adh--ml-left ()
  "Build the left island: what you are doing."
  (if (mode-line-window-selected-p)
      (adh--ml-island
       " "
       (when-let* ((m (adh--segment-modal))) (concat m "  "))
       (adh--ml-sub (adh--segment-file) (adh--segment-position))
       (when-let* ((modes (adh--segment-modes))) (concat "  " modes))
       (when-let* ((tabs (adh--segment-tab))) (concat "  " tabs)))
    (adh--ml-island " " (adh--segment-file))))

(defun adh--ml-compose ()
  "Assemble the mode line: left island, gap, right island."
  (let ((left  (adh--ml-left))
        (right (adh--ml-right)))
    (if (not right)
        left
      (list (or left "")
            (propertize " " 'display
                        `(space :align-to
                                (- (+ right right-fringe right-margin)
                                   ,(string-width (format-mode-line right)))))
            right))))

(defun adh--ml-render ()
  "Render the mode line, quietened while the minibuffer is active."
  (if (active-minibuffer-window)
      (let ((adh--ml-accent adh--ml-muted)
            (adh--ml-fg     adh--ml-muted)
            (adh--ml-warn   adh--ml-muted)
            (adh--ml-pill   adh--ml-isle)
            (adh--ml-strong adh--ml-isle))
        (adh--ml-compose))
    (adh--ml-compose)))

(defun adh--ml-minibuffer-refresh ()
  "Force every mode line to redraw, for `adh--ml-render'."
  (force-mode-line-update t))

(add-hook 'minibuffer-setup-hook #'adh--ml-minibuffer-refresh)
(add-hook 'minibuffer-exit-hook  #'adh--ml-minibuffer-refresh)

(setq-default mode-line-format '("%e" (:eval (adh--ml-render))))

(defun adh--ml-flatten-faces ()
  "Make the mode line background match the frame and drop its border."
  (dolist (face '(mode-line mode-line-active mode-line-inactive))
    (set-face-attribute face nil
                        :box nil
                        :overline nil
                        :underline nil
                        :background adh--ml-bg
                        :foreground adh--ml-muted)))

(defun adh--ml-refresh-theme (&rest _)
  "Re-read the palette, then re-flatten the mode line faces."
  (adh--ml-sync-palette)
  (adh--ml-flatten-faces))

(adh--ml-refresh-theme)
(add-hook 'enable-theme-functions #'adh--ml-refresh-theme)

(provide 'adh-modeline)
