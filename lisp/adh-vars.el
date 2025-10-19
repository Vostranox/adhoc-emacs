;;; -*- lexical-binding: t; coding: utf-8 -*-

(defgroup adhoc nil
  "AdHoc Emacs group."
  :group 'applications)

(defcustom adh-use-custom-keybinds t
  "When non-nil, load the built-in AdHoc modal and global keybindings.
Set to nil if you want to define your own layout from scratch."
  :group 'adhoc
  :type '(choice (const :tag "Use AdHoc Custom Layout" t)
                 (const :tag "Disable (Write Your Own)" nil)))

(defcustom adh-completion-backend 'company
  "In-buffer completion backend to use: `company' or `corfu'."
  :group 'adhoc
  :type '(choice (const :tag "Corfu" corfu)
                 (const :tag "Company" company)))

(defcustom adh-window-decoration nil
  "When non-nil, show window-manager title bar and borders on frames."
  :group 'adhoc
  :type '(choice (const :tag "Decorated" t)
                 (const :tag "Undecorated" nil)))

(defcustom adh-frame-opacity 100
  "Frame opacity as a percentage, 0 (transparent) to 100 (opaque)."
  :group 'adhoc
  :type 'integer)

(defcustom adh-mono-spaced-font
  (if (eq system-type 'windows-nt)
      "JetBrainsMono NFM Medium"
    "JetBrainsMono Nerd Font Mono")
  "Default monospaced font family for the `default' face."
  :group 'adhoc
  :type 'string)

(defcustom adh-mono-spaced-font-size 80
  "Default font height in 1/10 pt (80 = 8pt) for the `default' face."
  :group 'adhoc
  :type 'integer)

(defcustom adh-tmux-cd-session nil
  "Default tmux session or pane that `adh-tmux-cd' targets."
  :group 'adhoc
  :type '(choice (const :tag "Current session" nil) string))

(provide 'adh-vars)
