;;; -*- lexical-binding: t; coding: utf-8 -*-

(eval-when-compile
  (when (bound-and-true-p byte-compile-current-file)
    (require 'transient)))
(require 'adh-functions)

(defconst adh--settings-options
  '(adh-completion-style adh-completion-ui adh-completion-keys adh-use-lsp adh-use-vc adh-subwords adh-use-which-key adh-lsp-diagnostics
    adh-lsp-format-on-save adh-use-dirvish adh-window-decoration adh-frame-opacity
    adh-list-max-height adh-mono-spaced-font adh-mono-spaced-font-size)
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

(defun adh-settings ()
  "Change completion, LSP and display settings; S saves them for the next start."
  (interactive)
  (require 'transient)
  (call-interactively #'adh-settings))

(with-eval-after-load 'transient
  (transient-define-prefix adh-settings ()
    "Change completion, LSP and display settings; S saves them for the next start."
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
       :description (lambda () (adh--settings-choice 'adh-completion-keys 'tab-and-enter)))]
     ["LSP" :if (lambda () (featurep 'adh-eglot))
      ("l" adh-toggle-lsp
       :description (lambda () (adh--settings-switch "servers" 'adh-use-lsp)))
      ("d" adh-toggle-lsp-diagnostics
       :description (lambda () (adh--settings-switch "diagnostics" 'adh-lsp-diagnostics)))
      ("s" adh-toggle-lsp-format-on-save
       :description (lambda () (adh--settings-switch "format on save" 'adh-lsp-format-on-save)))]
     ["VC" :if (lambda () (featurep 'adh-core-packages))
      ("g" adh-toggle-vc
       :description (lambda () (adh--settings-switch "vc" 'adh-use-vc)))]
     ["Display"
      ("k" adh-toggle-subwords
       :description (lambda () (adh--settings-switch "subwords" 'adh-subwords))
       :if (lambda () (featurep 'adh-core-packages)))
      ("W" adh-toggle-which-key
       :description (lambda () (adh--settings-switch "which-key" 'adh-use-which-key))
       :if (lambda () (featurep 'adh-core-packages)))
      ("r" adh-toggle-dirvish
       :description (lambda () (adh--settings-switch "dirvish" 'adh-use-dirvish))
       :if (lambda () (featurep 'adh-ext-packages)))
      ("w" adh-toggle-window-decoration
       :description (lambda () (adh--settings-gui-only (adh--settings-switch "window decorations" 'adh-window-decoration))))
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
       :description (lambda () (adh--settings-gui-only (adh--settings-value "font size" 'adh-mono-spaced-font-size))))]]
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
