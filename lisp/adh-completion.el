;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh-enable-corfu-auto ()
  (interactive)
  (setq corfu-auto t
        adh--corfu-mode-line-name adh--corfu-auto-mode-line-string)
  (global-corfu-mode 0)
  (global-corfu-mode 1))

(defun adh-disable-corfu-auto ()
  (interactive)
  (setq corfu-auto nil
        adh--corfu-mode-line-name adh--corfu-mode-line-string)
  (global-corfu-mode 0)
  (global-corfu-mode 1))

(defun adh-toggle-corfu-auto ()
  (interactive)
  (if corfu-auto
      (adh-disable-corfu-auto)
    (adh-enable-corfu-auto)))

(use-package cape
  :ensure t
  :config
  (defalias 'adh--capf-history (cape-capf-properties #'cape-history :annotation-function (lambda (&rest _) "")))
  (defalias 'adh--capf-dabbrev (cape-capf-properties #'cape-dabbrev :annotation-function (lambda (&rest _) "")))
  (defalias 'adh--capf-abbrev (cape-capf-properties #'cape-abbrev :annotation-function (lambda (&rest _) "")))
  (defalias 'adh--capf-file (cape-capf-properties #'cape-file :annotation-function (lambda (&rest _) "")))
  (defalias 'adh--capf-keyword (cape-capf-properties #'cape-keyword :annotation-function (lambda (&rest _) "")))
  (defalias 'adh--capf-elisp-block (cape-capf-properties #'cape-elisp-block :annotation-function (lambda (&rest _) "")))

  (dolist (fn '(adh--capf-history
                adh--capf-dabbrev
                adh--capf-abbrev
                adh--capf-file
                adh--capf-keyword
                adh--capf-elisp-block))
    (add-hook 'completion-at-point-functions fn 'append))

  (with-eval-after-load 'eglot
    (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-preselect 'first)
  (corfu-on-exact-match nil)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay nil)
  (corfu-popupinfo-max-height 500)
  (corfu-popupinfo-max-width 500)
  (corfu-max-width 150)
  :config
  (defconst adh--corfu-mode-line-string " corfu")
  (defconst adh--corfu-auto-mode-line-string " corfu-auto")

  (defvar adh--corfu-mode-line-name adh--corfu-mode-line-string)
  (add-to-list 'minor-mode-alist '(global-corfu-mode adh--corfu-mode-line-name))

  (setf (alist-get 'internal-border-width corfu--frame-parameters) 0
        (alist-get 'child-frame-border-width corfu--frame-parameters) 0)
  (setq corfu-bar-width 0.0
        corfu-right-margin-width 0.0)
  (put 'corfu--bar 'corfu--bmp nil)

  (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'corfu-history))

  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)

  (dolist (hook '(comint-mode-hook
                  compilation-mode-hook
                  eshell-mode-hook
                  minibuffer-setup-hook
                  shell-mode-hook
                  term-mode-hook))
    (add-hook hook
              (lambda ()
                (setq-local corfu-auto nil)
                (corfu-mode 1)))))

(use-package kind-icon
  :ensure t :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
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
