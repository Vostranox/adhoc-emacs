;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh-set-completion-ui (ui)
  "Switch the in-buffer completion UI."
  (interactive (list (intern (completing-read "Select completion UI: " '(company corfu)))))
  (when (bound-and-true-p global-corfu-mode) (global-corfu-mode 0))
  (when (bound-and-true-p global-company-mode) (global-company-mode 0))
  (setq adh-completion-ui ui)
  (message "[adh] Completion UI set to %s" ui))

(defun adh-complete-at-point ()
  "Complete at point: trigger company manually if active, else `completion-at-point'."
  (interactive)
  (unless (and (bound-and-true-p company-mode)
               (company-manual-begin))
    (completion-at-point)))

(defun adh--cmp-auto-p ()
  "Return non-nil when automatic popup completion is on for the active UI."
  (cond ((eq adh-completion-ui 'company)
         (bound-and-true-p global-company-mode))
        ((eq adh-completion-ui 'corfu)
         (bound-and-true-p global-corfu-mode))))

(defun adh--set-cmp-auto (on)
  "Turn automatic popup completion for the active UI ON or off."
  (let ((arg (if on 1 -1)))
    (cond ((eq adh-completion-ui 'company)
           (global-company-mode arg))
          ((eq adh-completion-ui 'corfu)
           (global-corfu-mode arg)))))

(defun adh-toggle-cmp-auto ()
  "Toggle automatic popup completion for the active UI."
  (interactive)
  (adh--set-cmp-auto (not (adh--cmp-auto-p))))

(use-package company
  :ensure t :defer t
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 3))

(use-package company-posframe
  :ensure t :after company
  :custom
  (company-posframe-quickhelp-delay nil)
  :config
  (adh--rename-mode 'company-posframe-mode "")
  :hook
  (company-mode . company-posframe-mode))

(use-package corfu
  :ensure t :defer t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 3)
  (global-corfu-minibuffer nil)
  (corfu-preselect 'first)
  (corfu-on-exact-match nil)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay nil)
  :config
  (defconst adh--corfu-mode-line-string " corfu")
  (add-to-list 'minor-mode-alist '(corfu-mode adh--corfu-mode-line-string))

  (setf (alist-get 'internal-border-width corfu--frame-parameters) 0
        (alist-get 'child-frame-border-width corfu--frame-parameters) 0)
  (setq corfu-bar-width 0.0 corfu-right-margin-width 0.0)
  (put 'corfu--bar 'corfu--bmp nil)

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

(use-package kind-icon
  :ensure t
  :config
  (with-eval-after-load 'company
    (let* ((kind-func (lambda (cand) (company-call-backend 'kind cand)))
           (formatter (kind-icon-margin-formatter `((company-kind . ,kind-func)))))
      (defun adh--company-kind-icon-margin (cand _selected)
        (funcall formatter cand))
      (setq company-format-margin-function #'adh--company-kind-icon-margin)))
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
  (setq kind-icon-mapping
        '((array          "a"   :icon "symbol-array"       :face font-lock-type-face              :collection "vscode")
          (boolean        "b"   :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "vscode")
          (color          "#"   :icon "symbol-color"       :face success                          :collection "vscode")
          (command        "cm"  :icon "chevron-right"      :face default                          :collection "vscode")
          (constant       "co"  :icon "symbol-constant"    :face font-lock-constant-face          :collection "vscode")
          (class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "vscode")
          (constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
          (enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
          (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
          (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
          (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "vscode")
          (field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode")
          (file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "vscode")
          (folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "vscode")
          (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
          (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "vscode")
          (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "vscode")
          (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
          (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "vscode")
          (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
          (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
          (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "vscode")
          (operator       "op"  :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "vscode")
          (param          "pa"  :icon "gear"               :face default                          :collection "vscode")
          (property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "vscode")
          (reference      "rf"  :icon "library"            :face font-lock-variable-name-face     :collection "vscode")
          (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "vscode")
          (string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "vscode")
          (struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "vscode")
          (text           "tx"  :icon "symbol-key"         :face font-lock-doc-face               :collection "vscode")
          (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
          (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
          (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face          :collection "vscode")
          (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
          (variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode")
          (t              "."   :icon "question"           :face font-lock-warning-face           :collection "vscode"))))

(provide 'adh-completion)
