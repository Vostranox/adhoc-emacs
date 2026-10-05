;;; -*- lexical-binding: t; coding: utf-8 -*-

(eval-when-compile
  (when (bound-and-true-p byte-compile-current-file)
    (require 'transient)))
(require 'adh-functions)
(require 'adh-startup)

(defconst adh--settings-options
  '(adh-completion-style adh-completion-ui adh-completion-keys adh-vertico-style
    adh-use-lsp adh-use-vc adh-subwords adh-use-electric-pair adh-use-which-key adh-use-flycheck
    adh-flycheck-annotate adh-flycheck-annotate-style
    adh-lsp-format-on-save adh-use-dirvish adh-window-decoration adh-frame-opacity
    adh-list-max-height adh-mono-spaced-font adh-mono-spaced-font-size adh-auto-compile-config)
  "Options the settings menu shows and saves.")

(defun adh--settings-unsaved-p (var)
  "Return non-nil if VAR was changed this session and differs from its saved value.
Values set in the pre- or post-init files do not count."
  (and (get var 'customized-value)
       (not (equal (symbol-value var)
                   (eval (car (or (get var 'saved-value) (get var 'standard-value))) t)))))

(defun adh--settings-mark (var description)
  "Append a * to DESCRIPTION when VAR is not saved."
  (if (adh--settings-unsaved-p var)
      (concat description (propertize " *" 'face 'warning))
    description))

(defun adh--settings-label (label on)
  "Return LABEL, highlighted when ON."
  (propertize label 'face (if on 'transient-value 'transient-inactive-value)))

(defun adh--settings-switch (label var)
  "Return LABEL with boolean option VAR's state."
  (let ((on (symbol-value var)))
    (adh--settings-mark var (concat label " " (adh--settings-label (if on "on" "off") on)))))

(defun adh--settings-value (label var)
  "Return LABEL with option VAR's value."
  (adh--settings-mark var (concat label " " (propertize (format "%s" (symbol-value var))
                                                     'face 'transient-value))))

(defun adh--settings-choice (var value &optional label)
  "Return LABEL, or VALUE's name, highlighted when VAR is VALUE."
  (let* ((on (eq (symbol-value var) value))
         (desc (adh--settings-label (or label (symbol-name value)) on)))
    (if on (adh--settings-mark var desc) desc)))

(defun adh--settings-gui-only (description)
  "Mark DESCRIPTION as GUI only on terminal frames."
  (if (display-graphic-p)
      description
    (concat description " " (propertize "(GUI only)" 'face 'transient-inactive-value))))

(defun adh--settings-save-all ()
  "Save the settings changed this session for the next start, after confirming."
  (when (y-or-n-p "Save settings? ")
    (mapc #'customize-mark-to-save (seq-filter #'adh--settings-unsaved-p adh--settings-options))
    (let ((inhibit-message t))
      (custom-save-all))
    (message "[adh] Settings saved")))

(defun adh--settings-restore-defaults ()
  "Reset every settings option to its default for this session, after confirming.
LSP goes first, so servers are not restarted just before being shut down."
  (when (y-or-n-p "Restore defaults? ")
    (let (failed)
      (dolist (var (cons 'adh-use-lsp (remq 'adh-use-lsp adh--settings-options)))
        (condition-case err
            (customize-set-variable var (eval (car (get var 'standard-value)) t))
          (error (push (format "%s (%s)" var (error-message-string err)) failed))))
      (if failed
          (message "[adh] Defaults restored except %s" (string-join (nreverse failed) ", "))
        (message "[adh] Defaults restored; S saves them")))))

(defun adh--settings-read-integer (prompt default min &optional max)
  "Read an integer from MIN to MAX (no upper bound if nil) with PROMPT and DEFAULT."
  (let ((n (read-number prompt default)))
    (unless (and (integerp n) (>= n min) (or (null max) (<= n max)))
      (user-error "Enter a whole number %s" (if max (format "from %d to %d" min max)
                                              (format "of at least %d" min))))
    n))

(defun adh--toggle-setting (var label)
  "Flip boolean option VAR for this session and report it as LABEL."
  (customize-set-variable var (not (symbol-value var)))
  (message "[adh] %s %s" label (if (symbol-value var) "on" "off")))

(defun adh-toggle-lsp ()
  "Toggle LSP servers in supported buffers."
  (interactive)
  (adh--toggle-setting 'adh-use-lsp "LSP"))

(defun adh-toggle-flycheck ()
  "Toggle Flycheck diagnostics and Eglot integration."
  (interactive)
  (adh--toggle-setting 'adh-use-flycheck "Flycheck"))

(defun adh-toggle-flycheck-annotate ()
  "Toggle Flycheck diagnostic text shown beside or below the code."
  (interactive)
  (adh--toggle-setting 'adh-flycheck-annotate "Diagnostic text"))

(defun adh-set-flycheck-annotate-style (style)
  "Put the current line's inline diagnostic below the line or at its end per STYLE."
  (interactive
   (list (intern (completing-read "Diagnostic position: " '("below" "eol" "sideline")
                                  nil t nil nil (symbol-name adh-flycheck-annotate-style)))))
  (customize-set-variable 'adh-flycheck-annotate-style style)
  (message "[adh] Diagnostic position: %s" style))

(defun adh-toggle-lsp-format-on-save ()
  "Toggle formatting via LSP on save."
  (interactive)
  (adh--toggle-setting 'adh-lsp-format-on-save "LSP format on save"))

(defun adh-toggle-vc ()
  "Toggle the built-in VC backends and diff-hl."
  (interactive)
  (adh--toggle-setting 'adh-use-vc "VC"))

(defun adh-toggle-subwords ()
  "Toggle subword motion and display."
  (interactive)
  (adh--toggle-setting 'adh-subwords "Subwords"))

(defun adh-toggle-electric-pair ()
  "Toggle automatic matching brackets and quotes."
  (interactive)
  (adh--toggle-setting 'adh-use-electric-pair "Auto pairs"))

(defun adh-toggle-which-key ()
  "Toggle the which-key list of keys that follow a prefix."
  (interactive)
  (adh--toggle-setting 'adh-use-which-key "Which-key"))

(defun adh-toggle-dirvish ()
  "Toggle opening Dired buffers in Dirvish."
  (interactive)
  (adh--toggle-setting 'adh-use-dirvish "Dirvish"))

(defun adh-toggle-window-decoration ()
  "Toggle window-manager frame decorations."
  (interactive)
  (adh--toggle-setting 'adh-window-decoration "Window decorations"))

(defun adh-toggle-auto-compile-config ()
  "Toggle checking and rebuilding stale configuration on future startups."
  (interactive)
  (adh--toggle-setting 'adh-auto-compile-config "Automatic config compilation"))

(defun adh-settings ()
  "Change AdHoc settings; S saves them for the next start."
  (interactive)
  (require 'transient)
  (call-interactively #'adh-settings))

(with-eval-after-load 'transient
  (transient-define-prefix adh-settings-completion ()
    "Choose completion behavior and layout; C-g returns to settings."
    :transient-suffix t
    [["Completion" :if (lambda () (featurep 'adh-completion))
      ("n" (lambda () (interactive) (customize-set-variable 'adh-completion-style 'none))
       :description (lambda () (adh--settings-choice 'adh-completion-style 'none)))
      ("m" (lambda () (interactive) (customize-set-variable 'adh-completion-style 'minimal))
       :description (lambda () (adh--settings-choice 'adh-completion-style 'minimal)))
      ("f" (lambda () (interactive) (customize-set-variable 'adh-completion-style 'full))
       :description (lambda () (adh--settings-choice 'adh-completion-style 'full)))]
     ["Completion UI" :if (lambda () (featurep 'adh-completion))
      ("p" (lambda () (interactive) (customize-set-variable 'adh-completion-ui 'popup))
       :description (lambda () (adh--settings-choice 'adh-completion-ui 'popup)))
      ("b" (lambda () (interactive) (customize-set-variable 'adh-completion-ui 'minibuffer))
       :description (lambda () (adh--settings-choice 'adh-completion-ui 'minibuffer)))
      ("v" (lambda () (interactive) (customize-set-variable 'adh-completion-ui 'minibuffer-vertical))
       :description (lambda () (adh--settings-choice 'adh-completion-ui 'minibuffer-vertical "vertical")))
      ("c" (lambda () (interactive) (customize-set-variable 'adh-completion-ui 'default))
       :description (lambda () (adh--settings-choice 'adh-completion-ui 'default "*Completions*")))]
     ["Popup keys" :if (lambda () (featurep 'adh-completion))
      ("a" (lambda () (interactive) (customize-set-variable 'adh-completion-keys 'tab-and-go))
       :description (lambda () (adh--settings-choice 'adh-completion-keys 'tab-and-go)))
      ("i" (lambda () (interactive) (customize-set-variable 'adh-completion-keys 'tab-only))
       :description (lambda () (adh--settings-choice 'adh-completion-keys 'tab-only)))
      ("e" (lambda () (interactive) (customize-set-variable 'adh-completion-keys 'tab-and-enter))
       :description (lambda () (adh--settings-choice 'adh-completion-keys 'tab-and-enter)))]]
    ["Minibuffer"
     ("V" adh-set-vertico-style
      :description (lambda () (adh--settings-value "vertico" 'adh-vertico-style))
      :if (lambda () (featurep 'adh-minibuffer)))])

  (transient-define-prefix adh-settings-display ()
    "Change appearance and diagnostic position; C-g returns to settings."
    :transient-suffix t
    [["Appearance"
      ("o" (lambda (opacity)
             (interactive (list (adh--settings-read-integer "Opacity (0-100): "
                                                            adh-frame-opacity 0 100)))
             (customize-set-variable 'adh-frame-opacity opacity))
       :description (lambda () (adh--settings-gui-only (adh--settings-value "opacity" 'adh-frame-opacity))))
      ("h" (lambda (lines)
             (interactive (list (adh--settings-read-integer "List height: " adh-list-max-height 1)))
             (customize-set-variable 'adh-list-max-height lines))
       :description (lambda () (adh--settings-value "list height" 'adh-list-max-height)))
      ("t" (lambda (family)
             (interactive (list (completing-read "Font: " (seq-uniq (font-family-list))
                                                 nil nil nil nil adh-mono-spaced-font)))
             (customize-set-variable 'adh-mono-spaced-font family))
       :description (lambda () (adh--settings-gui-only (adh--settings-value "font" 'adh-mono-spaced-font))))
      ("z" (lambda (height)
             (interactive (list (adh--settings-read-integer "Font size (1/10 pt): "
                                                            adh-mono-spaced-font-size 1)))
             (customize-set-variable 'adh-mono-spaced-font-size height))
       :description (lambda () (adh--settings-gui-only (adh--settings-value "font size" 'adh-mono-spaced-font-size))))]
     ["Diagnostics" :if (lambda () (featurep 'adh-flycheck))
      ("P" adh-set-flycheck-annotate-style
       :description (lambda () (adh--settings-value "position" 'adh-flycheck-annotate-style)))]])

  (transient-define-prefix adh-settings ()
    "Change AdHoc settings; S saves them for the next start."
    :transient-suffix t
    [["Code tools" :if (lambda () (or (featurep 'adh-eglot) (featurep 'adh-flycheck)))
      ("l" adh-toggle-lsp
       :description (lambda () (adh--settings-switch "lsp" 'adh-use-lsp))
       :if (lambda () (featurep 'adh-eglot)))
      ("d" adh-toggle-flycheck
       :description (lambda () (adh--settings-switch "diagnostic" 'adh-use-flycheck))
       :if (lambda () (featurep 'adh-flycheck)))
      ("A" adh-toggle-flycheck-annotate
       :description (lambda () (adh--settings-switch "inline diagnostic" 'adh-flycheck-annotate))
       :if (lambda () (featurep 'adh-flycheck)))
      ("s" adh-toggle-lsp-format-on-save
       :description (lambda () (adh--settings-switch "format on save" 'adh-lsp-format-on-save))
       :if (lambda () (featurep 'adh-eglot)))]
     ["Editing"
      ("g" adh-toggle-vc
       :description (lambda () (adh--settings-switch "vc" 'adh-use-vc))
       :if (lambda () (featurep 'adh-core-packages)))
      ("k" adh-toggle-subwords
       :description (lambda () (adh--settings-switch "subwords" 'adh-subwords))
       :if (lambda () (featurep 'adh-core-packages)))
      ("p" adh-toggle-electric-pair
       :description (lambda () (adh--settings-switch "auto pairs" 'adh-use-electric-pair))
       :if (lambda () (featurep 'adh-emacs)))
      ("W" adh-toggle-which-key
       :description (lambda () (adh--settings-switch "which-key" 'adh-use-which-key))
       :if (lambda () (featurep 'adh-core-packages)))
      ("r" adh-toggle-dirvish
       :description (lambda () (adh--settings-switch "dirvish" 'adh-use-dirvish))
       :if (lambda () (featurep 'adh-ext-packages)))
      ("w" adh-toggle-window-decoration
       :description (lambda () (adh--settings-gui-only (adh--settings-switch "window decorations" 'adh-window-decoration))))]
     ["Options"
      ("c" "completion..." adh-settings-completion :transient t
       :if (lambda () (or (featurep 'adh-completion) (featurep 'adh-minibuffer))))
      ("v" "display..." adh-settings-display :transient t)]]
    ["Startup"
     ("C" adh-toggle-auto-compile-config
      :description (lambda () (adh--settings-switch "auto compile" 'adh-auto-compile-config)))]
    [("R" (lambda () (interactive) (adh--settings-restore-defaults))
      :description "restore defaults")
     ("S" (lambda () (interactive) (adh--settings-save-all))
      :description (lambda ()
                     (let ((n (seq-count #'adh--settings-unsaved-p adh--settings-options)))
                       (if (zerop n) "save" (format "save (%d unsaved *)" n)))))]
    (interactive)
    (when (window-parameter nil 'window-side)
      (select-window (adh--main-window)))
    (transient-setup 'adh-settings)))

(provide 'adh-settings)
