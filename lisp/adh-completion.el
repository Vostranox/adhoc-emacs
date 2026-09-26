;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-vars)

(defvar corfu-auto)
(defvar corfu-mode)
(defvar corfu-map)
(defvar corfu-preselect)
(defvar corfu-cycle)
(defvar corfu--index)
(defvar corfu-auto-commands)
(defvar vertico-multiform-categories)
(defvar vertico-multiform-commands)

(defun adh--completion-preview-trim (&rest _)
  "Hide LSP label noise after the name in the preview, e.g. `push_back(…)'."
  (when-let* (((bound-and-true-p eglot--managed-mode))
              (ov (bound-and-true-p completion-preview--overlay))
              (str (overlay-get ov 'after-string))
              (i (string-match-p "[ (<]" str))
              ((not (get-text-property i 'display str))))
    (setq str (copy-sequence str))
    (put-text-property i (length str) 'display "" str)
    (overlay-put ov 'after-string str)))

(defun adh--completion-set-corfu-auto (on)
  "Make the Corfu popup open while typing when ON."
  (unless (eq (bound-and-true-p corfu-auto) (and on t))
    (setq corfu-auto (and on t))
    (when (featurep 'corfu)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when corfu-mode
            (corfu-mode -1)
            (corfu-mode 1)))))))

(defun adh--apply-completion-style (style)
  "Apply completion STYLE."
  (adh--completion-set-corfu-auto (eq style 'full))
  (if (eq style 'minimal)
      (global-completion-preview-mode 1)
    (when (bound-and-true-p global-completion-preview-mode)
      (global-completion-preview-mode -1)))
  (when (fboundp 'adh--eglot-sync)
    (adh--eglot-sync)))

(defun adh--completion-in-vertical-minibuffer (beg end table pred)
  "Complete BEG to END in the minibuffer, listing candidates vertically."
  (let ((vertico-multiform-categories nil)
        (vertico-multiform-commands nil))
    (consult-completion-in-region beg end table pred)))

(defun adh--apply-completion-ui (ui)
  "Show in-buffer completion in UI; see `adh-completion-ui'."
  (if (eq ui 'popup)
      (global-corfu-mode 1)
    (when (bound-and-true-p global-corfu-mode)
      (global-corfu-mode -1)))
  (setq-default completion-in-region-function
                (pcase ui
                  ('minibuffer #'consult-completion-in-region)
                  ('minibuffer-vertical #'adh--completion-in-vertical-minibuffer)
                  (_ #'completion--in-region))))

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
    (when (featurep 'corfu)
      (adh--apply-corfu-keymap keys))))

(defun adh--apply-corfu-keymap (keys)
  "Bind the popup's TAB and RET for KEYS; see `adh-completion-keys'."
  (let ((go (eq keys 'tab-and-go)))
    (keymap-set corfu-map "TAB" (if go #'corfu-next #'corfu-insert))
    (keymap-set corfu-map "<tab>" (if go #'corfu-next #'corfu-insert))
    (keymap-set corfu-map "<backtab>" (and go #'corfu-previous))
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
  :ensure nil :defer t
  :config
  (add-to-list 'completion-preview-commands #'adh-backward-delete-char-dwim)
  (add-hook 'completion-preview-inhibit-functions
            (lambda () (bound-and-true-p completion-in-region-mode)))
  (advice-add 'completion-preview--make-overlay :after #'adh--completion-preview-trim)
  (advice-add 'completion-preview-next-candidate :after #'adh--completion-preview-trim))
(with-eval-after-load 'corfu-auto
  (add-to-list 'corfu-auto-commands #'adh-backward-delete-char-dwim))
(with-eval-after-load 'yasnippet
  (add-hook 'yas-keymap-disable-hook
            (lambda () (bound-and-true-p completion-in-region-mode))))

(use-package corfu
  :ensure t :defer t
  :custom
  (global-corfu-minibuffer nil)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay nil)
  :config
  (add-to-list 'minor-mode-alist '(corfu-mode (corfu-auto " corfu")))
  (adh--apply-corfu-keymap adh-completion-keys)
  (dolist (p '(alpha alpha-background))
    (setf (alist-get p corfu--frame-parameters) 100))
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
  :ensure t :defer t
  :init
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
  :custom
  (kind-icon-default-face 'font-lock-function-name-face)
  :config
  (setq kind-icon-default-style (append kind-icon-default-style '(:collection "vscode")))
  (setq kind-icon-mapping
        '((array          "a"   :icon "symbol-array")
          (boolean        "b"   :icon "symbol-boolean")
          (color          "#"   :icon "symbol-color")
          (command        "cm"  :icon "chevron-right")
          (constant       "co"  :icon "symbol-constant")
          (class          "c"   :icon "symbol-class")
          (constructor    "cn"  :icon "symbol-method")
          (enum           "e"   :icon "symbol-enum")
          (enum-member    "em"  :icon "symbol-enum-member")
          (event          "ev"  :icon "symbol-event")
          (field          "fd"  :icon "symbol-field")
          (file           "f"   :icon "symbol-file")
          (folder         "d"   :icon "folder")
          (function       "f"   :icon "symbol-method")
          (interface      "if"  :icon "symbol-interface")
          (keyword        "kw"  :icon "symbol-keyword")
          (macro          "mc"  :icon "symbol-misc")
          (magic          "ma"  :icon "lightbulb-autofix")
          (method         "m"   :icon "symbol-method")
          (module         "{"   :icon "symbol-namespace")
          (numeric        "nu"  :icon "symbol-numeric")
          (operator       "op"  :icon "symbol-operator")
          (param          "pa"  :icon "gear")
          (property       "pr"  :icon "symbol-property")
          (reference      "rf"  :icon "library")
          (snippet        "S"   :icon "symbol-snippet")
          (string         "s"   :icon "symbol-string")
          (struct         "%"   :icon "symbol-structure")
          (text           "tx"  :icon "symbol-key")
          (type-parameter "tp"  :icon "symbol-parameter")
          (unit           "u"   :icon "symbol-ruler")
          (value          "v"   :icon "symbol-enum")
          (variable       "va"  :icon "symbol-variable")
          (t              "."   :icon "question"))))

(provide 'adh-completion)
