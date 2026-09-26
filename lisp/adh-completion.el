;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-vars)

(defvar corfu-auto)
(defvar corfu-mode)
(defvar corfu-map)
(defvar corfu-preselect)
(defvar corfu-cycle)
(defvar corfu--index)
(defvar vertico-multiform-categories)
(defvar vertico-multiform-commands)

(defun adh--completion-preview-trim (&rest _)
  "Hide LSP label noise after the name in the preview, e.g. `push_back(…)'."
  (when-let* ((ov (bound-and-true-p completion-preview--overlay))
              (str (overlay-get ov 'after-string))
              (i (string-match-p "[ (<]" str))
              ((not (get-text-property i 'display str))))
    (setq str (copy-sequence str))
    (put-text-property i (length str) 'display "" str)
    (overlay-put ov 'after-string str)))

(defun adh--completion-set-corfu-auto (on)
  "Make the Corfu popup open while typing when ON."
  (unless (eq corfu-auto (and on t))
    (setq corfu-auto (and on t))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when corfu-mode
          (corfu-mode -1)
          (corfu-mode 1))))))

(defun adh--apply-completion-style (style)
  "Apply completion STYLE."
  (adh--completion-set-corfu-auto (eq style 'full))
  (global-completion-preview-mode (if (eq style 'minimal) 1 -1))
  (adh--eglot-sync))

(defun adh--completion-in-minibuffer (beg end table pred)
  "Complete BEG to END in the minibuffer, vertically for `minibuffer-vertical'."
  (if (eq adh-completion-ui 'minibuffer-vertical)
      (let ((vertico-multiform-categories nil)
            (vertico-multiform-commands nil))
        (consult-completion-in-region beg end table pred))
    (consult-completion-in-region beg end table pred)))

(defun adh--apply-completion-ui (ui)
  "Show in-buffer completion in UI; see `adh-completion-ui'."
  (global-corfu-mode (if (eq ui 'popup) 1 -1))
  (setq-default completion-in-region-function
                (if (memq ui '(minibuffer minibuffer-vertical))
                    #'adh--completion-in-minibuffer
                  #'completion--in-region)))

(defun adh-complete-at-point ()
  "Complete at point; the Corfu popup skips inserting the common prefix."
  (interactive)
  (if (bound-and-true-p corfu-mode)
      (let ((completion-in-region-function
             (lambda (beg end table pred)
               (corfu--setup beg end table pred)
               t)))
        (completion-at-point))
    (completion-at-point)))

(defun adh--apply-completion-keys (keys)
  "Set how the Corfu popup selects and accepts; see `adh-completion-keys'."
  (let ((go (eq keys 'tab-and-go)))
    (setq corfu-preselect (if go 'prompt 'first)
          corfu-cycle go)
    (keymap-set corfu-map "TAB" (if go #'corfu-next #'corfu-insert))
    (keymap-set corfu-map "<tab>" (if go #'corfu-next #'corfu-insert))
    (keymap-set corfu-map "<backtab>" (and go #'corfu-previous))
    (keymap-set corfu-map "C-<return>" #'corfu-insert)
    (keymap-set corfu-map "RET"
                (pcase keys
                  ('tab-and-enter #'corfu-insert)
                  ('tab-only nil)
                  (_ `(menu-item "" corfu-insert
                                 :filter ,(lambda (cmd) (and (>= corfu--index 0) cmd))))))))

(defun adh-set-completion-style (style)
  "Set `adh-completion-style' to STYLE for this session."
  (interactive
   (list (intern (completing-read "Completion style: " '("none" "minimal" "full") nil t))))
  (customize-set-variable 'adh-completion-style style)
  (message "[adh] Completion style: %s" style))

(defun adh-set-completion-ui (ui)
  "Set `adh-completion-ui' to UI for this session."
  (interactive
   (list (intern (completing-read "Completion UI: "
                                  '("popup" "minibuffer" "minibuffer-vertical" "default")
                                  nil t))))
  (customize-set-variable 'adh-completion-ui ui)
  (message "[adh] Completion UI: %s" ui))

(defun adh-set-completion-keys (keys)
  "Set `adh-completion-keys' to KEYS for this session."
  (interactive
   (list (intern (completing-read "Popup keys: " '("tab-and-go" "tab-only" "tab-and-enter") nil t))))
  (customize-set-variable 'adh-completion-keys keys)
  (message "[adh] Popup keys: %s" keys))

(use-package completion-preview
  :ensure nil
  :config
  (add-hook 'completion-preview-inhibit-functions
            (lambda () (bound-and-true-p completion-in-region-mode)))
  (advice-add 'completion-preview--make-overlay :after #'adh--completion-preview-trim)
  (advice-add 'completion-preview-next-candidate :after #'adh--completion-preview-trim)
  (keymap-set completion-preview-active-mode-map "C-<return>" #'completion-preview-insert))
(with-eval-after-load 'yasnippet
  (add-hook 'yas-keymap-disable-hook
            (lambda () (bound-and-true-p completion-in-region-mode))))

(use-package corfu
  :ensure t :demand t
  :custom
  (corfu-auto nil)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 3)
  (global-corfu-minibuffer nil)
  (corfu-on-exact-match nil)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay nil)
  (corfu-border-width 1)
  (corfu-right-margin-width 0.5)
  :config
  (defconst adh--corfu-mode-line-string " corfu")
  (add-to-list 'minor-mode-alist '(corfu-auto adh--corfu-mode-line-string))
  (setf (alist-get 'alpha-background corfu--frame-parameters) 100)
  (define-advice corfu--make-frame (:filter-return (frame) adh-fringe)
    (when (frame-live-p frame)
      (set-face-background 'fringe (face-background 'corfu-default nil t) frame))
    frame)
  (corfu-echo-mode 1)
  (corfu-popupinfo-mode 1))

(use-package cape
  :ensure t
  :config
  (defalias 'adh-cape-keyword-dabbrev-no-ann
    (cape-capf-properties (cape-capf-super #'cape-keyword #'cape-dabbrev) :annotation-function #'ignore))
  (defalias 'adh-cape-file-no-ann (cape-capf-properties #'cape-file :annotation-function #'ignore))
  (add-hook 'completion-at-point-functions #'adh-cape-keyword-dabbrev-no-ann)
  (add-hook 'completion-at-point-functions #'adh-cape-file-no-ann)
  (add-hook 'eglot-managed-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'adh-cape-file-no-ann nil t))))

(adh--apply-completion-ui adh-completion-ui)
(adh--apply-completion-keys adh-completion-keys)
(adh--apply-completion-style adh-completion-style)

(use-package kind-icon
  :ensure t
  :config
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
  (setq kind-icon-mapping
        '((array          "a"   :icon "symbol-array"       :face font-lock-function-name-face :collection "vscode")
          (boolean        "b"   :icon "symbol-boolean"     :face font-lock-function-name-face :collection "vscode")
          (color          "#"   :icon "symbol-color"       :face font-lock-function-name-face :collection "vscode")
          (command        "cm"  :icon "chevron-right"      :face font-lock-function-name-face :collection "vscode")
          (constant       "co"  :icon "symbol-constant"    :face font-lock-function-name-face :collection "vscode")
          (class          "c"   :icon "symbol-class"       :face font-lock-function-name-face :collection "vscode")
          (constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face :collection "vscode")
          (enum           "e"   :icon "symbol-enum"        :face font-lock-function-name-face :collection "vscode")
          (enummember     "em"  :icon "symbol-enum-member" :face font-lock-function-name-face :collection "vscode")
          (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-function-name-face :collection "vscode")
          (event          "ev"  :icon "symbol-event"       :face font-lock-function-name-face :collection "vscode")
          (field          "fd"  :icon "symbol-field"       :face font-lock-function-name-face :collection "vscode")
          (file           "f"   :icon "symbol-file"        :face font-lock-function-name-face :collection "vscode")
          (folder         "d"   :icon "folder"             :face font-lock-function-name-face :collection "vscode")
          (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face :collection "vscode")
          (interface      "if"  :icon "symbol-interface"   :face font-lock-function-name-face :collection "vscode")
          (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-function-name-face :collection "vscode")
          (macro          "mc"  :icon "lambda"             :face font-lock-function-name-face :collection "vscode")
          (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-function-name-face :collection "vscode")
          (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face :collection "vscode")
          (module         "{"   :icon "file-code-outline"  :face font-lock-function-name-face :collection "vscode")
          (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-function-name-face :collection "vscode")
          (operator       "op"  :icon "symbol-operator"    :face font-lock-function-name-face :collection "vscode")
          (param          "pa"  :icon "gear"               :face font-lock-function-name-face :collection "vscode")
          (property       "pr"  :icon "symbol-property"    :face font-lock-function-name-face :collection "vscode")
          (reference      "rf"  :icon "library"            :face font-lock-function-name-face :collection "vscode")
          (snippet        "S"   :icon "symbol-snippet"     :face font-lock-function-name-face :collection "vscode")
          (string         "s"   :icon "symbol-string"      :face font-lock-function-name-face :collection "vscode")
          (struct         "%"   :icon "symbol-structure"   :face font-lock-function-name-face :collection "vscode")
          (text           "tx"  :icon "symbol-key"         :face font-lock-function-name-face :collection "vscode")
          (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-function-name-face :collection "vscode")
          (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-function-name-face :collection "vscode")
          (unit           "u"   :icon "symbol-ruler"       :face font-lock-function-name-face :collection "vscode")
          (value          "v"   :icon "symbol-enum"        :face font-lock-function-name-face :collection "vscode")
          (variable       "va"  :icon "symbol-variable"    :face font-lock-function-name-face :collection "vscode")
          (t              "."   :icon "question"           :face font-lock-function-name-face :collection "vscode"))))

(provide 'adh-completion)
