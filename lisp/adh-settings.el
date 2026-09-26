;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'transient)
(require 'adh-vars)

(defconst adh--settings-options
  '(adh-completion-style adh-completion-ui adh-completion-keys adh-use-lsp adh-use-vc adh-subwords adh-lsp-diagnostics
    adh-lsp-format-on-save adh-use-dirvish adh-window-decoration adh-frame-opacity
    adh-list-max-height adh-mono-spaced-font adh-mono-spaced-font-size)
  "Options the settings menu shows and saves.")

(defun adh--settings-unsaved-p (var)
  "Return non-nil if VAR differs from its saved value, or default if never saved."
  (not (equal (symbol-value var)
              (eval (car (or (get var 'saved-value) (get var 'standard-value))) t))))

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
  (let ((on (eq (symbol-value var) value)))
    (funcall (if on (apply-partially #'adh--settings-mark var) #'identity)
             (adh--settings-label (or label (symbol-name value)) on))))

(defun adh--settings-gui-only (description)
  "Mark DESCRIPTION as GUI only on terminal frames."
  (if (display-graphic-p)
      description
    (concat description " " (propertize "(GUI only)" 'face 'transient-inactive-value))))

(defun adh--settings-save-all ()
  "Save every settings option for the next start, after confirming."
  (when (y-or-n-p "Save settings? ")
    (mapc #'customize-mark-to-save adh--settings-options)
    (let ((inhibit-message t))
      (custom-save-all))
    (message "[adh] Settings saved")))

(defun adh--settings-restore-defaults ()
  "Reset every settings option to its default for this session, after confirming."
  (when (y-or-n-p "Restore default settings? ")
    (dolist (var adh--settings-options)
      (customize-set-variable var (eval (car (get var 'standard-value)) t)))
    (message "[adh] Defaults restored; S saves them")))

(defun adh--toggle-setting (var label)
  "Flip boolean option VAR for this session and report it as LABEL."
  (customize-set-variable var (not (symbol-value var)))
  (message "[adh] %s %s" label (if (symbol-value var) "on" "off")))

(defun adh-toggle-lsp ()
  "Toggle LSP servers in supported buffers."
  (interactive)
  (adh--toggle-setting 'adh-use-lsp "LSP"))

(defun adh-toggle-lsp-diagnostics ()
  "Toggle LSP diagnostics in flymake."
  (interactive)
  (adh--toggle-setting 'adh-lsp-diagnostics "LSP diagnostics"))

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

(defun adh-toggle-dirvish ()
  "Toggle opening Dired buffers in Dirvish."
  (interactive)
  (adh--toggle-setting 'adh-use-dirvish "Dirvish"))

(defun adh-toggle-window-decoration ()
  "Toggle window-manager frame decorations."
  (interactive)
  (adh--toggle-setting 'adh-window-decoration "Window decorations"))

(transient-define-prefix adh-settings ()
  "Change completion, LSP and display settings; S saves them for the next start."
  [["Completion"
    ("n" (lambda () (interactive) (customize-set-variable 'adh-completion-style 'none))
     :description (lambda () (adh--settings-choice 'adh-completion-style 'none)) :transient t)
    ("m" (lambda () (interactive) (customize-set-variable 'adh-completion-style 'minimal))
     :description (lambda () (adh--settings-choice 'adh-completion-style 'minimal)) :transient t)
    ("f" (lambda () (interactive) (customize-set-variable 'adh-completion-style 'full))
     :description (lambda () (adh--settings-choice 'adh-completion-style 'full)) :transient t)]
   ["Completion UI"
    ("p" (lambda () (interactive) (customize-set-variable 'adh-completion-ui 'popup))
     :description (lambda () (adh--settings-choice 'adh-completion-ui 'popup)) :transient t)
    ("b" (lambda () (interactive) (customize-set-variable 'adh-completion-ui 'minibuffer))
     :description (lambda () (adh--settings-choice 'adh-completion-ui 'minibuffer)) :transient t)
    ("v" (lambda () (interactive) (customize-set-variable 'adh-completion-ui 'minibuffer-vertical))
     :description (lambda () (adh--settings-choice 'adh-completion-ui 'minibuffer-vertical "vertical"))
     :transient t)
    ("c" (lambda () (interactive) (customize-set-variable 'adh-completion-ui 'default))
     :description (lambda () (adh--settings-choice 'adh-completion-ui 'default "*Completions*"))
     :transient t)]
   ["Popup keys"
    ("a" (lambda () (interactive) (customize-set-variable 'adh-completion-keys 'tab-and-go))
     :description (lambda () (adh--settings-choice 'adh-completion-keys 'tab-and-go)) :transient t)
    ("i" (lambda () (interactive) (customize-set-variable 'adh-completion-keys 'tab-only))
     :description (lambda () (adh--settings-choice 'adh-completion-keys 'tab-only)) :transient t)
    ("e" (lambda () (interactive) (customize-set-variable 'adh-completion-keys 'tab-and-enter))
     :description (lambda () (adh--settings-choice 'adh-completion-keys 'tab-and-enter)) :transient t)]
   ["LSP"
    ("l" adh-toggle-lsp
     :description (lambda () (adh--settings-switch "servers" 'adh-use-lsp)) :transient t)
    ("d" adh-toggle-lsp-diagnostics
     :description (lambda () (adh--settings-switch "diagnostics" 'adh-lsp-diagnostics)) :transient t)
    ("s" adh-toggle-lsp-format-on-save
     :description (lambda () (adh--settings-switch "format on save" 'adh-lsp-format-on-save))
     :transient t)]
   ["VC"
    ("g" adh-toggle-vc
     :description (lambda () (adh--settings-switch "vc" 'adh-use-vc)) :transient t)]
   ["Display"
    ("k" adh-toggle-subwords
     :description (lambda () (adh--settings-switch "subwords" 'adh-subwords)) :transient t)
    ("r" adh-toggle-dirvish
     :description (lambda () (adh--settings-switch "dirvish" 'adh-use-dirvish)) :transient t)
    ("w" adh-toggle-window-decoration
     :description (lambda () (adh--settings-gui-only (adh--settings-switch "window decorations" 'adh-window-decoration)))
     :transient t)
    ("o" (lambda (opacity)
           (interactive (list (read-number "Frame opacity (0-100): " adh-frame-opacity)))
           (customize-set-variable 'adh-frame-opacity (max 0 (min 100 opacity))))
     :description (lambda () (adh--settings-gui-only (adh--settings-value "opacity" 'adh-frame-opacity)))
     :transient t)
    ("h" (lambda (lines)
           (interactive (list (read-number "List height: " adh-list-max-height)))
           (customize-set-variable 'adh-list-max-height (max 1 lines)))
     :description (lambda () (adh--settings-value "list height" 'adh-list-max-height)) :transient t)
    ("t" (lambda (family)
           (interactive (list (completing-read "Font: " (seq-uniq (font-family-list))
                                               nil nil nil nil adh-mono-spaced-font)))
           (customize-set-variable 'adh-mono-spaced-font family))
     :description (lambda () (adh--settings-gui-only (adh--settings-value "font" 'adh-mono-spaced-font)))
     :transient t)
    ("z" (lambda (height)
           (interactive (list (read-number "Font height (1/10 pt): " adh-mono-spaced-font-size)))
           (customize-set-variable 'adh-mono-spaced-font-size height))
     :description (lambda () (adh--settings-gui-only (adh--settings-value "font size" 'adh-mono-spaced-font-size)))
     :transient t)]]
  [("R" (lambda () (interactive) (adh--settings-restore-defaults))
    :description "restore defaults" :transient t)
   ("S" (lambda () (interactive) (adh--settings-save-all))
    :description (lambda ()
                   (let ((n (seq-count #'adh--settings-unsaved-p adh--settings-options)))
                     (if (zerop n) "save" (format "save (%d unsaved *)" n))))
    :transient t)])

(provide 'adh-settings)
