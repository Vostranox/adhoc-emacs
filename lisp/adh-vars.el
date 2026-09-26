;;; -*- lexical-binding: t; coding: utf-8 -*-

(defgroup adhoc nil
  "AdHoc Emacs group."
  :group 'applications)

(defun adh--custom-setter (apply)
  "Return a `:set' function that also calls APPLY with the value, once defined."
  (lambda (symbol value)
    (set-default symbol value)
    (when (fboundp apply)
      (funcall apply value))))

(defcustom adh-use-custom-keybinds t
  "When non-nil, load the AdHoc modal and global keybindings."
  :group 'adhoc
  :type '(choice (const :tag "Use AdHoc Custom Layout" t)
                 (const :tag "Disable (Write Your Own)" nil)))

(defcustom adh-use-dirvish t
  "When non-nil, Dired buffers open in Dirvish."
  :group 'adhoc
  :type '(choice (const :tag "Use Dirvish" t)
                 (const :tag "Plain Dired" nil))
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-use-dirvish))

(defcustom adh-completion-style 'none
  "What in-buffer completion does on its own while you type."
  :group 'adhoc
  :type '(choice (const :tag "No automatic completion" none)
                 (const :tag "Inline preview" minimal)
                 (const :tag "Popup, snippets and eldoc" full))
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-completion-style))

(defcustom adh-completion-ui 'default
  "Where in-buffer completion shows its candidates."
  :group 'adhoc
  :type '(choice (const :tag "Corfu popup" popup)
                 (const :tag "Minibuffer" minibuffer)
                 (const :tag "Vertical minibuffer" minibuffer-vertical)
                 (const :tag "*Completions* buffer" default))
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-completion-ui))

(defcustom adh-completion-keys 'tab-only
  "How the completion popup selects and accepts candidates.
`tab-and-go': nothing preselected; TAB/S-TAB move; RET accepts the selection.
`tab-only':   first preselected; TAB accepts; RET is a newline.
`tab-and-enter': first preselected; TAB and RET accept."
  :group 'adhoc
  :type '(choice (const :tag "TAB-and-Go" tab-and-go)
                 (const :tag "TAB only" tab-only)
                 (const :tag "TAB and Enter" tab-and-enter))
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-completion-keys))

(defcustom adh-use-lsp nil
  "When non-nil, start eglot in every buffer with a known LSP server."
  :group 'adhoc
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--lsp-set-autostart))

(defcustom adh-lsp-diagnostics nil
  "When non-nil, eglot feeds the LSP server's diagnostics to flymake."
  :group 'adhoc
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--lsp-set-diagnostics))

(defcustom adh-lsp-format-on-save nil
  "When non-nil, eglot formats managed buffers on save."
  :group 'adhoc
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--lsp-set-format-on-save))

(defcustom adh-use-vc nil
  "When non-nil, enable the built-in VC backends and `global-diff-hl-mode'."
  :group 'adhoc
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-vc))

(defcustom adh-subwords nil
  "When non-nil, word commands stop at camelCase subwords, which glasses marks."
  :group 'adhoc
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-subwords))

(defcustom adh-window-decoration nil
  "When non-nil, show window-manager title bar and borders on frames."
  :group 'adhoc
  :type '(choice (const :tag "Decorated" t)
                 (const :tag "Undecorated" nil))
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-window-decoration))

(defcustom adh-frame-opacity 100
  "Frame opacity as a percentage, 0 (transparent) to 100 (opaque)."
  :group 'adhoc
  :type 'integer
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-frame-opacity))

(defcustom adh-mono-spaced-font
  (if (eq system-type 'windows-nt)
      "JetBrainsMono NFM Medium"
    "JetBrainsMono Nerd Font Mono")
  "Default monospaced font family for the `default' face."
  :group 'adhoc
  :type 'string
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-font-settings))

(defcustom adh-mono-spaced-font-size 105
  "Default font height, in 1/10 pt."
  :group 'adhoc
  :type 'integer
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-font-settings))

(defcustom adh-project-root-markers '(".git" ".project")
  "File or directory names that mark a project root."
  :group 'adhoc
  :type '(repeat string))

(defcustom adh-tmux-cd-session nil
  "Default tmux session or pane that `adh-tmux-cd' targets."
  :group 'adhoc
  :type '(choice (const :tag "Current session" nil) string))

(defcustom adh-treesit-excluded-langs '(cmake)
  "Languages excluded from `treesit-auto' mode remapping."
  :group 'adhoc
  :type '(repeat symbol))

(defcustom adh-hidden-buffer-modes '(magit-mode dired-mode)
  "Parent modes whose buffers stay out of buffer switching."
  :group 'adhoc
  :type '(repeat symbol))

(defcustom adh-popup-buffers
  '("\\*Warnings\\*" "\\*Async Shell Command\\*" "Output\\*$"
    "\\*Embark Export" "^\\*e: " "\\*eldoc"
    help-mode apropos-mode messages-buffer-mode backtrace-mode
    compilation-mode comint-mode occur-mode xref--xref-buffer-mode
    flymake-diagnostics-buffer-mode flymake-project-diagnostics-mode
    embark-collect-mode)
  "Buffer name regexps or major modes shown in the bottom popup.
A mode also matches its derived modes."
  :group 'adhoc
  :type '(repeat (choice regexp symbol)))

(defcustom adh-list-max-height 25
  "Maximum lines shown by vertico, *Completions* and popups."
  :group 'adhoc
  :type 'integer
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-list-max-height))

(provide 'adh-vars)
