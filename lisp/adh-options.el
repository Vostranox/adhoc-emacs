;;; -*- lexical-binding: t; coding: utf-8 -*-

(defgroup adhoc nil
  "AdHoc Emacs group."
  :group 'applications)

(defun adh--custom-setter (apply)
  "Return a `:set' function that also calls APPLY when the value changes."
  (lambda (symbol value)
    (let ((old (and (default-boundp symbol) (default-value symbol))))
      (set-default symbol value)
      (when (and (fboundp apply) (not (equal old value)))
        (condition-case err
            (funcall apply value)
          (error (set-default symbol old)
                 (when (assq 'user (get symbol 'theme-value))
                   (custom-push-theme 'theme-value symbol 'user 'set (custom-quote old)))
                 (signal (car err) (cdr err))))))))

(defcustom adh-use-custom-keybinds t
  "When non-nil, load the AdHoc modal and global keybindings."
  :group 'adhoc
  :type '(choice (const :tag "Use custom layout" t)
                 (const :tag "Disable custom layout" nil)))

(defcustom adh-use-dirvish t
  "When non-nil, Dired buffers open in Dirvish."
  :group 'adhoc
  :type '(choice (const :tag "Use Dirvish" t)
                 (const :tag "Do not use Dirvish" nil))
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

(defcustom adh-completion-ui 'popup
  "Where in-buffer completion shows its candidates."
  :group 'adhoc
  :type '(choice (const :tag "Corfu popup" popup)
                 (const :tag "Minibuffer" minibuffer)
                 (const :tag "Vertical minibuffer" minibuffer-vertical)
                 (const :tag "*Completions* buffer" default))
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-completion-ui))

(defcustom adh-completion-keys 'tab-and-enter
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

(defcustom adh-vertico-style 'flat
  "Default layout for minibuffer completion.
Command-specific Vertico layouts take precedence."
  :group 'adhoc
  :type '(choice (const :tag "Flat list" flat)
                 (const :tag "Vertical list" vertical)
                 (const :tag "Grid" grid)
                 (const :tag "Reverse list" reverse)
                 (const :tag "Full-buffer list" buffer))
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-vertico-style))

(defcustom adh-use-picker nil
  "When non-nil, open the prompts of `adh-picker-commands' in a floating
picker that previews the current candidate."
  :group 'adhoc
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-picker))

(defcustom adh-use-lsp nil
  "When non-nil, start eglot in every buffer with a known LSP server."
  :group 'adhoc
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--lsp-set-autostart))

(defcustom adh-use-flycheck nil
  "When non-nil, enable Flycheck and its Eglot bridge."
  :group 'adhoc
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-flycheck))

(defcustom adh-flycheck-annotate t
  "When non-nil, show inline diagnostics when Flycheck is enabled."
  :group 'adhoc
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-flycheck-annotate))

(defcustom adh-flycheck-annotate-style 'eol
  "Where the current line's inline diagnostic goes."
  :group 'adhoc
  :type '(choice (const :tag "Below the line" below)
                 (const :tag "End of the line" eol)
                 (const :tag "Right edge of the window" sideline))
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-flycheck-annotate-style))

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

(defcustom adh-use-electric-pair nil
  "When non-nil, automatically insert matching brackets and quotes."
  :group 'adhoc
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-electric-pair))

(defcustom adh-trim-trailing-whitespace t
  "When non-nil, trim trailing whitespace on save except in Markdown.
Set this buffer-locally to nil for other whitespace-sensitive files."
  :group 'adhoc
  :type 'boolean
  :safe #'booleanp)

(defcustom adh-use-which-key nil
  "When non-nil, list the keys that can follow a prefix key (which-key)."
  :group 'adhoc
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (adh--custom-setter 'adh--apply-which-key))

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

(defcustom adh-tmux-session "quake:dev"
  "Default tmux target of the `adh-tmux-' commands.
A session name, or SESSION:WINDOW or SESSION:WINDOW.PANE."
  :group 'adhoc
  :type '(choice (const :tag "Current session" nil) string))

(defcustom adh-treesit-excluded-modes '(cmake-ts-mode)
  "Tree-sitter modes left out of `treesit-enabled-modes'."
  :group 'adhoc
  :type '(repeat symbol))

(defcustom adh-treesit-ensured-langs '(bash c c-sharp cpp go json lua python rust toml yaml zig)
  "Tree-sitter grammars that `adh-treesit-ensure-grammars' installs if missing.
Other grammars are installed the first time a file needs them."
  :group 'adhoc
  :type '(repeat symbol))

(defcustom adh-hidden-buffer-modes '(magit-mode dired-mode)
  "Parent modes whose buffers stay out of buffer switching."
  :group 'adhoc
  :type '(repeat symbol))

(defcustom adh-popup-buffers
  '("\\*Warnings\\*" "\\*Async Shell Command\\*" "Output\\*$"
    "^\\*adh-compile-config\\*$"
    "\\*Embark Export" "^\\*e: " "\\*eldoc"
    help-mode apropos-mode messages-buffer-mode backtrace-mode
    compilation-mode comint-mode occur-mode xref--xref-buffer-mode
    flycheck-error-list-mode flycheck-error-message-mode
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

(provide 'adh-options)

;;; adh-options.el ends here
