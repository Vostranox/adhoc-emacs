;;; -*- lexical-binding: t; coding: utf-8 -*-

(defmacro => (&rest body)
  "Wrap BODY in an anonymous interactive command, for inline keybindings."
  `(lambda () (interactive) ,@body))

(defmacro adh-keymap-set (keymap &rest bindings)
  "Bind keys in KEYMAP.
Each element of BINDINGS is (KEY DEFINITION) or (KEY DEFINITION NAME),
where NAME is an optional which-key label."
  (declare (indent 1))
  `(progn
     ,@(mapcan
        (lambda (binding)
          (pcase-let ((`(,key ,def ,name) binding))
            (cons
             `(keymap-set ,keymap ,key ,def)
             (when name
               `((with-eval-after-load 'which-key
                   (which-key-add-keymap-based-replacements
                     ,keymap ,key ,name)))))))
        bindings)))

(defmacro adh-defkeymap (name &rest body)
  "Define keymap NAME, fill it with bindings, optionally attach to a parent.
Keywords may precede the bindings:
  :map PARENT    keymap to attach NAME into
  :prefix KEY    prefix key for NAME in PARENT
  :label LABEL   which-key label for the prefix binding
The remaining BODY is bindings as in `adh-keymap-set'."
  (declare (indent 1))
  (let (parent key label)
    (while (keywordp (car body))
      (pcase (pop body)
        (:map    (setq parent (pop body)))
        (:prefix (setq key (pop body)))
        (:label  (setq label (pop body)))
        (kw (error "adh-defkeymap: unknown keyword %S" kw))))
    (when (xor parent key)
      (error "adh-defkeymap: :map and :prefix must be used together"))
    `(progn
       (defvar-keymap ,name)
       (adh-keymap-set ,name ,@body)
       ,@(when parent
           `((adh-keymap-set ,parent
               (,key ,name ,@(and label (list label)))))))))

;;; global map

(adh-keymap-set global-map
  ("<backspace>" #'adh-backward-delete-char-dwim))

;; C
(adh-keymap-set global-map
  ("C-l" #'recenter-top-bottom)
  ("C-d" #'other-window)
  ("C-b" #'adh-select-side-window)
  ("C-n" #'consult-isearch-history)
  ("C-r" #'adh-isearch-backward-with-region)
  ("C-t" #'adh-keyboard-quit-dwim)
  ("C-s" #'adh-isearch-forward-with-region)
  ("C-g" #'adh-keyboard-quit-dwim)
  ("C-f" #'adh-complete-at-point)
  ("C-o" #'recentf-open)
  ("C-u" #'clipboard-yank)
  ("C-S-u" #'consult-yank-pop)
  ("C-h" #'mark-word)
  ("C-a" #'next-line)
  ("C-e" #'previous-line)
  ("C-i" #'indent-for-tab-command)
  ("C-S-i" #'tab-to-tab-stop)
  ("C-," #'adh-duplicate-dwim)
  ("C-." #'embark-act)
  ("C-+" #'global-text-scale-adjust)
  ("C-<" #'text-scale-decrease)
  ("C->" #'text-scale-increase))

;; M
(adh-keymap-set global-map
  ("M-r" #'adh-consult-point-to-register)
  ("M-t" #'transpose-words)
  ("M-o" #'zoxide-travel)
  ("M-u" #'indent-region)
  ("M-h" #'previous-buffer)
  ("M-a" #'adh-move-lines-down)
  ("M-e" #'adh-move-lines-up)
  ("M-i" #'next-buffer)
  ("M-p" #'adh-project-switch-to-dired)
  ("M-," #'xref-go-back)
  ("M-." #'xref-find-definitions)
  ("M-/" #'xref-find-references)
  ("M-<" #'end-of-buffer)
  ("M->" #'beginning-of-buffer))

;; C-M
(adh-keymap-set global-map
  ("C-M-f" #'downcase-dwim)
  ("C-M-o" #'capitalize-dwim)
  ("C-M-u" #'upcase-dwim)
  ("C-M-." #'adh-apropos))

;; C-c
(adh-keymap-set global-map
  ("C-c f" #'mc/edit-beginnings-of-lines)
  ("C-c o" #'mc/mark-all-dwim)
  ("C-c u" #'mc/insert-numbers)
  ("C-c A" #'org-agenda)
  ("C-c E" #'org-capture))

;; C-c C
(adh-keymap-set global-map
  ("C-c C-r" #'adh-wrap-region-with-pair)
  ("C-c C-u" #'vundo)
  ("C-c C-h" #'help-command))

;; C-c C-t
(adh-keymap-set global-map
  ("C-c C-t f" #'adh-toggle-eglot-flymake)
  ("C-c C-t o" #'flymake-show-buffer-diagnostics)
  ("C-c C-t h" #'adh-toggle-cmp-auto)
  ("C-c C-t a" #'adh-toggle-eglot-global)
  ("C-c C-t e" #'adh-toggle-eglot-format-on-save)
  ("C-c C-t i" #'adh-subword-toggle)
  ("C-c C-t ." #'adh-toggle-ide-mode)
  ("C-c C-t /" #'adh-toggle-func-case-at-point))

;; C-x
(adh-keymap-set global-map
  ("C-x b" #'switch-to-buffer)
  ("C-x j" (=> (find-file (adh--get-project-dir))) "find-file-project-root")
  ("C-x f" #'find-file-at-point)
  ("C-x h" #'mark-whole-buffer)
  ("C-x x i" #'insert-buffer)
  ("C-x ," #'next-error)
  ("C-x ." #'previous-error))

;; C-x C
(adh-keymap-set global-map
  ("C-x C-b" #'list-buffers)
  ("C-x C-w" #'write-file)
  ("C-x C-f" #'find-file)
  ("C-x C-j" #'dired-jump)
  ("C-x C-h" #'mark-whole-buffer))

;; C-x RET
(adh-keymap-set global-map
  ("C-x <return> f" #'set-buffer-file-coding-system)
  ("C-x <return> o" #'adh-show-buffer-file-encoding))

;;; minibuffer map

(adh-keymap-set minibuffer-local-map
  ("<backspace>" #'adh-backward-delete-char-dwim)
  ("C-x b" (=> (adh--minibuffer-pivot #'switch-to-buffer)) "switch-to-buffer")
  ("C-r" #'consult-history)
  ("C-u" #'clipboard-yank)
  ("C-S-u" #'consult-yank-pop)
  ("C-h" #'mark-word)
  ("C-o" (=> (adh--minibuffer-pivot #'recentf-open)) "recentf-open")
  ("M-o" (=> (adh--minibuffer-pivot #'zoxide-travel)) "zoxide-travel")
  ("M-a" #'embark-export)
  ("C-x C-b" (=> (adh--minibuffer-pivot #'list-buffers)) "list-buffers"))

(adh-keymap-set minibuffer-local-shell-command-map
  ("C-a" #'adh-minibuffer-next-history-or-clear)
  ("C-e" #'previous-history-element))

;;; leader map

(adh-defkeymap adh-leader-map
  :map global-map
  :prefix "C-x C-o"
  ("l" #'tab-switch)
  ("d" #'adh-consult-jump-to-register)
  ("c" #'adh-switch-to-buffer)
  ("b" #'bookmark-jump)
  ("x" #'adh-toggle-meow-motion-mode)
  ("w" #'adh-dired-or-file)
  ("DEL" #'find-file))

(adh-defkeymap adh-find-keymap
  :map adh-leader-map
  :prefix "s"
  ("f" #'adh-consult-fd-here)
  ("o" #'adh-consult-fd-directories-here)
  ("h" #'adh-consult-fd-project)
  ("a" #'adh-consult-fd-directories-project)
  ("e" #'adh-consult-fd-all)
  ("," #'adh-get-executable)
  ("." #'adh-consult-locate)
  ("/" #'adh-getenv))

(adh-defkeymap adh-search-keymap
  :map adh-leader-map
  :prefix "t"
  ("f" #'adh-consult-ripgrep-here)
  ("o" #'adh-consult-imenu)
  ("h" #'adh-consult-ripgrep-project)
  ("a" #'adh-consult-line-with-region)
  ("e" #'adh-consult-ripgrep-all))

(adh-defkeymap adh-replace-keymap
  :map adh-leader-map
  :prefix "r"
  ("f" #'query-replace)
  ("h" #'vr/query-replace)
  ("a" #'vr/replace)
  ("e" #'vr/mc-mark))

(adh-defkeymap adh-compile-keymap
  :map adh-leader-map
  :prefix "n"
  ("f" #'compile)
  ("o" #'async-shell-command)
  ("u" #'adh-compile-region)
  ("h" #'adh-project-compile)
  ("a" #'adh-project-async-shell-command)
  ("e" #'adh-project-compile-region)
  ("." #'recompile))

(adh-defkeymap adh-magit-keymap
  :map adh-leader-map
  :prefix "m"
  ("SPC" #'adh-magit-log-trace-region-or-line)
  ("c" #'magit-find-file)
  ("s" #'magit-file-checkout)
  ("m" #'magit)
  ("j" #'magit-file-dispatch)
  ("f" #'adh-toggle-magit-blame)
  ("o" #'adh-magit-log-buffer-file-follow)
  ("u" #'magit-ediff-show-commit)
  ("y" #'magit-dispatch)
  ("h" #'adh-magit-staging-quick)
  ("a" #'magit-log-current)
  ("e" #'magit-checkout)
  ("i" #'adh-magit-switch-or-status)
  ("," #'magit-git-command-topdir)
  ("." #'magit-status-quick)
  ("/" #'adh-magit-restore-current))

(adh-defkeymap adh-file-keymap
  :map adh-leader-map
  :prefix "f"
  ("r" #'adh-copy-file-name)
  ("t" #'adh-copy-path)
  ("s" #'adh-copy-full-path)
  ("x" #'ediff-files))

(adh-defkeymap adh-window-keymap
  :map adh-leader-map
  :prefix "h"
  ("l" #'kill-buffer-and-window)
  ("d" #'adh-delete-other-windows)
  ("c" #'delete-window)
  ("n" #'balance-windows)
  ("r" #'windower-toggle-split)
  ("t" #'split-window-vertically)
  ("T" #'adh-split-below-root)
  ("s" #'split-window-horizontally)
  ("S" #'adh-split-right-root)
  ("w" #'windower-swap)
  ("m" #'window-toggle-side-windows)
  ("x" #'adh-to-side-window))

(adh-defkeymap adh-buffer-keymap
  :map adh-leader-map
  :prefix "a"
  ("l" #'kill-buffer)
  ("d" #'adh-kill-other-buffers)
  ("c" #'kill-current-buffer)
  ("b" #'adh-kill-matching-buffers-no-ask-except-current)
  ("n" #'align-regexp)
  ("r" #'rename-buffer)
  ("g" #'revert-buffer)
  ("x" #'ediff-buffers)
  ("m" #'eval-buffer)
  ("w" #'eval-region)
  ("h" #'ibuffer)
  ("a" #'scratch-buffer))

(adh-defkeymap adh-tab-keymap
  :map adh-leader-map
  :prefix "e"
  ("d" #'tab-close-other)
  ("c" #'tab-close)
  ("r" #'tab-rename))

(adh-defkeymap adh-bookmark-keymap
  :map adh-leader-map
  :prefix "i"
  ("c" #'bookmark-delete)
  ("r" #'bookmark-rename)
  ("s" #'bookmark-set))

(adh-defkeymap adh-tmux-keymap
  :map adh-leader-map
  :prefix "z"
  ("h" #'adh-tmux-to-emacs-buffer)
  ("a" #'adh-tmux-to-emacs-buffer-all)
  ("e" #'adh-tmux-cd))

;;; modal mode

(with-eval-after-load 'meow
  (defun adh--meow-minibuffer-update-cursor ()
    "Match the minibuffer cursor shape to the current meow state."
    (setq cursor-type
          (cond
           ((meow-normal-mode-p) 'box)
           ((meow-insert-mode-p) 'bar)
           ((meow-motion-mode-p) 'box)
           (t 'bar))))

  (defun adh--meow-minibuffer-setup ()
    "Enable modal editing in the minibuffer, starting in insert state."
    (meow-insert-mode 1)
    (setq-local cursor-type 'bar)
    (add-hook 'post-command-hook #'adh--meow-minibuffer-update-cursor nil t)
    (redisplay)
    (local-set-key (kbd "<escape>") #'meow-normal-mode)
    (local-set-key (kbd "C-t") #'adh-keyboard-quit-dwim))

  (add-hook 'minibuffer-setup-hook #'adh--meow-minibuffer-setup)
  (setq meow-update-cursor-functions-alist (assq-delete-all 'minibufferp meow-update-cursor-functions-alist))

  (adh-keymap-set global-map
    ("C-x C-z" meow-normal-state-keymap))

  (meow-define-keys 'normal
    (cons "SPC" adh-leader-map)
    '("<escape>" . adh-mc-keyboard-quit-dwim)
    '("7" . mc/unmark-next-like-this)
    '("8" . mc/mark-next-like-this)
    '("9" . mc/mark-previous-like-this)
    '("0" . mc/unmark-previous-like-this)
    '("j" . beginning-of-visual-line)
    '("f" . back-to-indentation)
    '("o" . end-of-visual-line)
    '("u" . clipboard-yank)
    '("y" . repeat)
    '("h" . backward-char)
    '("a" . next-line)
    '("e" . previous-line)
    '("i" . forward-char)
    '("H" . join-line)
    '("A" . adh-insert-line-below)
    '("E" . adh-insert-line-above)
    '("I" . adh-join-line-above)
    '("k" . adh-mark-line)
    '("p" . mark-word)
    '("." . backward-paragraph)
    '("," . forward-paragraph)
    '("/" . mark-paragraph)
    '("2" . mc/skip-to-previous-like-this)
    '("3" . mc/skip-to-next-like-this)
    '("l" . clipboard-kill-ring-save)
    '("d" . backward-word)
    '("c" . forward-word)
    '("b" . undo)
    '("B" . undo-redo)
    '("n" . set-mark-command)
    '("r" . exchange-point-and-mark)
    '("t" . adh-meow-insert)
    '("s" . adh-meow-insert-replace)
    '("g g" . adh-consult-goto-line)
    '("g h" . adh-avy-goto-line-indent)
    '("z" . kill-word)
    '("x" . kill-whole-line)
    '("X" . adh-kill-line-above)
    '("m" . kill-sexp)
    '("M" . kill-rectangle)
    '("w" . adh-kill-region-or-line)
    '("v" . rectangle-mark-mode)
    '("V" . string-insert-rectangle)
    '("#" . adh-mark-inside)
    '("$" . mark-sexp)
    '("!" . set-mark-command)
    '("+" . beginning-of-buffer)
    '("-" . end-of-buffer)
    '("?" . adh-meow-insert-replace)
    '(";" . mark-sexp)
    '(")" . adh-scroll-up-half)
    '("(" . adh-scroll-down-half)
    '("{" . pop-to-mark-command)
    '("=" . backward-up-list)
    '(">" . backward-sexp)
    '("<" . forward-sexp)
    '("_" . adh-down-list)
    '(":" . comment-line)
    '("[" . end-of-defun)
    '("]" . beginning-of-defun)
    '("\\" . mark-defun))

  (meow-define-keys 'motion
    (cons "SPC" adh-leader-map)
    '("<escape>" . adh-mc-keyboard-quit-dwim)
    '("a" . next-line)
    '("e" . previous-line)
    '("(" . adh-scroll-down-half)
    '(")" . adh-scroll-up-half)))

;;; packages

(adh-keymap-set completion-in-region-mode-map
  ("<return>" #'minibuffer-choose-completion)
  ("<tab>" #'minibuffer-choose-completion)
  ("C-<return>" #'minibuffer-choose-completion)
  ("C-s" #'adh-completion-in-region-isearch)
  ("C-a" #'minibuffer-next-completion)
  ("C-e" #'minibuffer-previous-completion))

(adh-keymap-set Buffer-menu-mode-map
  ("<return>" #'Buffer-menu-other-window)
  ("<backspace>" (=> (adh--with-saved-window #'Buffer-menu-other-window)))
  ("+" #'beginning-of-buffer)
  ("-" #'end-of-buffer)
  ("C-d" #'other-window)
  ("C-o" #'recentf)
  ("M-o" #'zoxide-travel))

(with-eval-after-load 'vertico
  (adh-keymap-set vertico-multiform-map
    ("<left>" #'backward-char)
    ("<right>" #'forward-char)
    ("C-<return>" #'vertico-exit)
    ("C-f" #'vertico-multiform-vertical)
    ("C-a" #'vertico-next)
    ("C-e" #'vertico-previous)))

(with-eval-after-load 'vertico-reverse
  (when (boundp 'vertico-reverse-map)
    (adh-keymap-set vertico-reverse-map
      ("C-a" #'vertico-previous)
      ("C-e" #'vertico-next))))

(with-eval-after-load 'completion-preview
  (adh-keymap-set completion-preview-active-mode-map
    ("<return>" #'completion-preview-insert)
    ("<tab>" #'completion-preview-insert)
    ("C-<return>" #'completion-preview-insert)
    ("C-a" #'completion-preview-next-candidate)
    ("C-e" #'completion-preview-previous-candidate)))

(with-eval-after-load 'company
  (adh-keymap-set company-active-map
    ("<return>" #'company-complete-selection)
    ("<tab>" #'company-complete-selection)
    ("C-<return>" #'company-complete-selection)
    ("C-a" #'company-select-next)
    ("C-e" #'company-select-previous)))

(with-eval-after-load 'corfu
  (adh-keymap-set corfu-map
    ("<return>" #'corfu-insert)
    ("<tab>" #'corfu-insert)
    ("C-SPC" #'corfu-insert-separator)
    ("C-<return>" #'corfu-insert)
    ("C-a" #'corfu-next)
    ("C-e" #'corfu-previous)))

(with-eval-after-load 'multiple-cursors
  (adh-keymap-set mc/keymap
    ("<return>" nil)
    ("<escape>" #'adh-mc-keyboard-quit-dwim)
    ("C-t" #'mc/keyboard-quit)
    ("C-g" #'mc/keyboard-quit)))

(with-eval-after-load 'isearch
  (adh-keymap-set isearch-mode-map
    ("C-d" #'avy-isearch)
    ("C-t" #'isearch-abort)
    ("C-u" #'isearch-yank-x-selection)
    ("M-a" #'adh-isearch-occur)))

(with-eval-after-load 'dired
  (adh-keymap-set dired-mode-map
    ("<backspace>" #'dired-display-file)
    ("<return>" #'dired-find-file-other-window)
    ("l" #'clipboard-kill-ring-save)
    ("s" #'adh-dired-sort-toggle-or-edit)
    ("h" #'dired-up-directory)
    ("i" #'dired-find-file)
    ("C-o" #'recentf-open)
    ("C-," #'adh-dired-duplicate-dwim)
    ("M-o" #'zoxide-travel)
    ("M-a" #'dired-toggle-read-only)))

(with-eval-after-load 'wdired
  (adh-keymap-set wdired-mode-map
    ("M-a" #'wdired-abort-changes)))

(with-eval-after-load 'org
  (adh-keymap-set org-mode-map
    ("C-," #'adh-duplicate-dwim)
    ("M-h" #'previous-buffer)))

(with-eval-after-load 'org-agenda
  (adh-keymap-set org-agenda-mode-map
    ("m" #'org-agenda-month-view)))

(with-eval-after-load 'xref
  (adh-keymap-set xref--xref-buffer-mode-map
    ("<backspace>" #'xref-show-location-at-point)))

(with-eval-after-load 'compile
  (adh-keymap-set compilation-mode-map
    ("+" #'beginning-of-buffer)
    ("-" #'end-of-buffer)
    ("." #'previous-error-no-select)
    ("," #'next-error-no-select)
    ("l" #'clipboard-kill-ring-save)
    ("C-o" #'recentf-open)
    ("M-o" #'zoxide-travel))

  (adh-keymap-set compilation-button-map
    ("<backspace>" #'compilation-display-error)))

(with-eval-after-load 'flymake
  (adh-keymap-set flymake-diagnostics-buffer-mode-map
    ("<backspace>" #'adh-flymake-display-diagnostic)
    ("C-o" #'recentf-open)
    ("M-o" #'zoxide-travel)))

(with-eval-after-load 'grep
  (adh-keymap-set grep-mode-map
    ("." #'previous-error-no-select)
    ("," #'next-error-no-select)
    ("M-a" #'wgrep-change-to-wgrep-mode)))

(with-eval-after-load 'wgrep
  (adh-keymap-set wgrep-mode-map
    ("M-a" #'wgrep-abort-changes)))

(with-eval-after-load 'replace
  (adh-keymap-set occur-mode-map
    ("<backspace>" #'occur-mode-display-occurrence)
    ("." #'previous-error-no-select)
    ("," #'next-error-no-select)
    ("C-o" #'recentf-open)
    ("M-o" #'zoxide-travel)
    ("M-a" #'occur-edit-mode))

  (adh-keymap-set occur-edit-mode-map
    ("M-a" #'occur-cease-edit)))

(with-eval-after-load 'vundo
  (adh-keymap-set vundo-mode-map
    ("C-t" #'vundo-quit)))

(with-eval-after-load 'transient
  (adh-keymap-set transient-base-map
    ("<escape>" #'transient-quit-one)))

(with-eval-after-load 'shell
  (adh-keymap-set shell-command-mode-map
    ("q" (=> (quit-window t)) "quit-window")
    ("C-d" #'other-window)
    ("C-o" #'recentf)
    ("M-o" #'zoxide-travel)))

(with-eval-after-load 'magit
  (adh-keymap-set magit-mode-map
    ("<return>" #'adh-magit-visit-thing-other-window)
    ("<backspace>" #'adh-magit-preview-thing)
    ("," #'magit-section-forward)
    ("." #'magit-section-backward)
    ("C-a" #'magit-next-line)
    ("C-e" #'magit-previous-line)
    ("M-1" #'magit-section-show-level-1-all)
    ("M-2" #'magit-section-show-level-2-all)
    ("M-3" #'magit-section-show-level-3-all)
    ("M-4" #'magit-section-show-level-4-all))

  (adh-keymap-set magit-mode-map
    ("C-c m l" #'magit-smerge-keep-lower)
    ("C-c m c" #'magit-smerge-keep-current)
    ("C-c m b" #'magit-smerge-keep-base)
    ("C-c m u" #'magit-smerge-keep-upper)
    ("C-c m a" #'magit-smerge-keep-all))

  (adh-keymap-set magit-blame-mode-map
    ("M-<return>" #'adh-magit-show-commit-original)
    ("," #'magit-blame-next-chunk)
    ("." #'magit-blame-previous-chunk)
    ("w" #'magit-blame-copy-hash)
    ("C-w" #'adh-magit-blame-copy-short-hash)))

(with-eval-after-load 'git-rebase
  (when (boundp 'git-rebase-mode-map)
    (adh-keymap-set git-rebase-mode-map
      ("M-a" #'git-rebase-move-line-down)
      ("M-e" #'git-rebase-move-line-up))))

(with-eval-after-load 'ibuffer
  (adh-keymap-set ibuffer-mode-map
    ("<return>" #'ibuffer-visit-buffer-other-window)
    ("<backspace>" #'ibuffer-visit-buffer-other-window-noselect)
    ("i" #'ibuffer-visit-buffer)
    ("C-d" #'other-window)
    ("C-o" #'recentf)
    ("M-o" #'zoxide-travel)))

(provide 'adh-keybinds)
