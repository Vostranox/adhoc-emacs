;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh-set-completion-backend (backend)
  (interactive (list (intern (completing-read "Select backend: " '(company corfu)))))
  (when (bound-and-true-p global-corfu-mode) (global-corfu-mode 0))
  (when (bound-and-true-p global-company-mode) (global-company-mode 0))
  (setq adh-completion-backend backend)
  (message "[adh] Completion backend set to %s" backend))

(defun adh-complete-at-point ()
  (interactive)
  (unless (and (bound-and-true-p company-mode)
               (company-manual-begin))
    (completion-at-point)))

(defun adh-toggle-cmp-auto ()
  (interactive)
  (cond ((eq adh-completion-backend 'company)
         (global-company-mode 'toggle))
        ((eq adh-completion-backend 'corfu)
         (global-corfu-mode 'toggle))))

(use-package company
  :ensure t :defer t
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 3)
  :config
  (adh--rename-mode 'company-mode " company"))

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

  (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'corfu-history))

  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1))

(use-package cape
  :ensure t
  :config
  (defalias 'cape-dabbrev-no-ann (cape-capf-properties #'cape-dabbrev :annotation-function (lambda (_cand) "")))
  (defalias 'cape-file-no-ann (cape-capf-properties #'cape-file :annotation-function (lambda (_cand) "")))
  (defalias 'cape-history-no-ann (cape-capf-properties #'cape-history :annotation-function (lambda (_cand) "")))
  (add-hook 'completion-at-point-functions #'cape-dabbrev-no-ann)
  (add-hook 'completion-at-point-functions #'cape-file-no-ann)
  (add-hook 'completion-at-point-functions #'cape-history-no-ann)
  (with-eval-after-load 'eglot
    (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)))

(use-package kind-icon
  :ensure t
  :config
  (with-eval-after-load 'company
    (let* ((kind-func (lambda (cand) (company-call-backend 'kind cand)))
           (formatter (kind-icon-margin-formatter `((company-kind . ,kind-func)))))
      (defun my-company-kind-icon-margin (cand _selected)
        (funcall formatter cand))
      (setq company-format-margin-function #'my-company-kind-icon-margin)))
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
