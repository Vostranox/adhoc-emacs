;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-functions)

(defvar meow-normal-state-keymap)
(defvar vertico-multiform-map)
(defvar org-agenda-mode-map)
(defvar magit-mode-map)
(defvar magit-hunk-section-map)
(defvar magit-blame-read-only-mode-map)
(defvar git-commit-mode-map)
(defvar git-rebase-mode-map)
(defvar ibuffer-mode-map)
(defvar ediff-mode-map)
(defvar electric-pair-mode-map)

(declare-function dirvish-subtree--expanded-p "dirvish-subtree")

;;; override map

(define-keymap :keymap adh-override-map
  "C-d" #'other-window
  "C-b" #'adh-select-popup
  "C-o" #'recentf-open
  "M-n" #'adh-project-compile-region
  "M-r" #'adh-consult-ripgrep-project
  "M-t" #'adh-project-async-shell-command-region
  "M-s" #'adh-consult-fd-project
  "M-o" #'zoxide-travel)

(dolist (hook '(prog-mode-hook nxml-mode-hook markdown-mode-hook markdown-ts-mode-hook))
  (add-hook hook (lambda () (use-local-map nil))))

;;; global map

(keymap-set global-map "<backspace>" #'adh-backward-delete-char-dwim)

;; C
(define-keymap :keymap global-map
  "C-l" #'kill-ring-save
  "C-r" #'adh-isearch-backward-with-region
  "C-t" #'adh-keyboard-quit-dwim
  "C-s" #'adh-isearch-forward-with-region
  "C-g" #'adh-keyboard-quit-dwim
  "C-f" #'adh-complete-at-point
  "C-u" #'clipboard-yank
  "C-S-u" #'consult-yank-pop
  "C-h" #'mark-word
  "C-a" #'next-line
  "C-e" #'previous-line
  "C-S-i" #'tab-to-tab-stop
  "C-." #'embark-act
  "C-," #'adh-duplicate-dwim
  "C-/" #'universal-argument
  "C-+" #'global-text-scale-adjust)

(keymap-set universal-argument-map "C-/" #'universal-argument-more)

;; M
(define-keymap :keymap global-map
  "M-l" #'recenter-top-bottom
  "M-u" #'indent-region
  "M-h" #'previous-buffer
  "M-a" #'adh-move-lines-down
  "M-e" #'adh-move-lines-up
  "M-i" #'next-buffer
  "M-/" #'xref-find-references
  "M-<" #'end-of-buffer
  "M->" #'beginning-of-buffer)

;; C-M
(define-keymap :keymap global-map
  "C-M-f" #'downcase-dwim
  "C-M-o" #'capitalize-dwim
  "C-M-u" #'upcase-dwim
  "C-M-i" #'string-inflection-elixir-style-cycle)

;; C-c
(define-keymap :keymap global-map
  "C-c f" #'mc/edit-beginnings-of-lines
  "C-c o" #'mc/mark-all-dwim
  "C-c u" #'mc/insert-numbers
  "C-c r" #'adh-wrap-region-with-pair)

;; C-c C
(keymap-set global-map "C-c C-h" #'help-command)

;; C-x
(define-keymap :keymap global-map
  "C-x d" #'adh-switch-dired-dwim
  "C-x b" #'switch-to-buffer
  "C-x g" #'adh-magit-status-dwim
  "C-x j" (cons "find-file-project-root"
                (=> (find-file (or (adh--get-project-dir) default-directory))))
  "C-x f" #'find-file-at-point
  "C-x u" #'vundo
  "C-x y" #'repeat
  "C-x h" #'mark-whole-buffer
  "C-x I" #'adh-tmux-insert-pane
  "C-x M-i" #'adh-tmux-insert-pane-all
  "C-x ." #'adh-settings)

;; C-x C
(keymap-set global-map "C-x C-h" #'mark-whole-buffer)

;; C-x RET
(keymap-set global-map "C-x RET f" #'set-buffer-file-coding-system)
(keymap-set global-map "C-x RET o" #'adh-show-buffer-file-encoding)

;; M-g
(keymap-set global-map "M-g i" #'consult-imenu-multi)

;;; minibuffer map

(define-keymap :keymap minibuffer-local-map
  "C-l" #'kill-ring-save
  "C-r" #'consult-history
  "C-o" (cons "recentf-open" (=> (adh--minibuffer-pivot #'recentf-open)))
  "C-h" #'mark-word
  "M-d" #'adh-consult-dirs-pivot
  "M-o" (cons "zoxide-travel" (=> (adh--minibuffer-pivot #'zoxide-travel)))
  "M-a" #'embark-export
  "M-." #'adh-consult-root-pivot
  "M-<" #'end-of-buffer
  "M->" #'minibuffer-beginning-of-buffer
  "C-x b" (cons "switch-to-buffer" (=> (adh--minibuffer-pivot #'switch-to-buffer)))
  "<backspace>" #'adh-backward-delete-char-dwim
  "<remap> <next-line>" #'next-line-or-history-element
  "<remap> <previous-line>" #'previous-line-or-history-element)

(with-eval-after-load 'consult
  (keymap-set consult-narrow-map "<backspace>" consult--narrow-delete))

(define-keymap :keymap minibuffer-local-shell-command-map
  "C-a" #'adh-minibuffer-next-history-or-clear
  "C-e" #'previous-history-element
  "M-d" #'adh-shell-command-dir-pivot
  "M-." #'adh-shell-command-root-pivot)

;;; leader map

(adh-defkeymap adh-leader-map
  :map global-map
  :prefix "C-x C-o"
  "l" #'tab-switch
  "c" #'adh-switch-to-buffer
  "b" #'bookmark-jump
  "x" #'adh-toggle-meow-motion-mode
  "w" #'adh-dired-or-file
  "DEL" #'find-file)

(adh-defkeymap adh-find-keymap
  :map adh-leader-map
  :prefix "s"
  "f" #'adh-consult-fd-here
  "h" #'adh-consult-fd-project
  "," #'adh-get-executable
  "." #'adh-consult-locate
  "/" #'adh-getenv)

(adh-defkeymap adh-replace-keymap
  :map adh-leader-map
  :prefix "t"
  "f" #'query-replace
  "h" #'vr/query-replace
  "a" #'vr/replace
  "e" #'vr/mc-mark)

(adh-defkeymap adh-search-keymap
  :map adh-leader-map
  :prefix "r"
  "f" #'adh-consult-ripgrep-here
  "h" #'adh-consult-ripgrep-project
  "a" #'consult-imenu-multi)

(adh-defkeymap adh-compile-keymap
  :map adh-leader-map
  :prefix "n"
  "f" #'adh-compile-region
  "o" #'adh-async-shell-command-region
  "h" #'adh-project-compile-region
  "a" #'adh-project-async-shell-command-region
  "e" #'adh-tmux-send-region
  "." #'recompile)

(adh-defkeymap adh-magit-keymap
  :map adh-leader-map
  :prefix "m"
  "o" #'adh-switch-magit-buffer
  "f" #'magit-file-dispatch
  "h" #'magit-dispatch
  "a" #'adh-magit-status-dwim
  "e" #'adh-magit-staging-quick
  "." #'adh-magit-status-dwim
  "/" #'magit-find-file)

(adh-defkeymap adh-file-keymap
  :map adh-leader-map
  :prefix "f"
  "r" #'adh-copy-file-name
  "t" #'adh-copy-path
  "s" #'adh-copy-full-path
  "x" #'ediff-files)

(adh-defkeymap adh-window-keymap
  :map adh-leader-map
  :prefix "h"
  "l" #'kill-buffer-and-window
  "d" #'adh-delete-other-windows
  "c" #'adh-delete-window
  "n" #'balance-windows
  "r" #'window-layout-rotate-clockwise
  "t" #'split-window-vertically
  "s" #'split-window-horizontally
  "m" #'popper-toggle
  "x" #'adh-popup-toggle-type)

(adh-defkeymap adh-buffer-keymap
  :map adh-leader-map
  :prefix "a"
  "l" #'kill-buffer
  "d" #'adh-kill-other-buffers
  "c" #'kill-current-buffer
  "b" #'adh-kill-matching-buffers-no-ask-except-current
  "n" #'align-regexp
  "r" #'rename-buffer
  "g" #'revert-buffer
  "x" #'ediff-buffers
  "m" #'eval-buffer
  "w" #'eval-region
  "a" #'adh-scratch-buffer)

(adh-defkeymap adh-tab-keymap
  :map adh-leader-map
  :prefix "e"
  "d" #'tab-close-other
  "c" #'tab-close
  "r" #'tab-rename)

(adh-defkeymap adh-bookmark-keymap
  :map adh-leader-map
  :prefix "i"
  "c" #'bookmark-delete
  "r" #'bookmark-rename
  "s" #'bookmark-set)

;;; modal mode

(with-eval-after-load 'meow
  (keymap-set global-map "C-x C-z" meow-normal-state-keymap)

  (meow-define-keys 'normal
    (cons "SPC" adh-leader-map)
    '("<escape>" . adh-mc-keyboard-quit-dwim)
    '("7" . mc/unmark-previous-like-this)
    '("8" . mc/mark-next-like-this)
    '("9" . mc/mark-previous-like-this)
    '("0" . mc/unmark-next-like-this)
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
    '("g g" . consult-goto-line)
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

(define-keymap :keymap completion-in-region-mode-map
  "<return>" (adh--completions-key #'adh-completion-choose)
  "<tab>" (adh--completions-key #'adh-completion-choose)
  "C-<return>" (adh--completions-key #'adh-completion-choose)
  "C-s" (adh--completions-key #'adh-completion-in-region-isearch)
  "C-a" (adh--completions-key #'minibuffer-next-completion)
  "C-e" (adh--completions-key #'minibuffer-previous-completion))

(define-keymap :keymap special-mode-map
  "+" #'beginning-of-buffer
  "-" #'end-of-buffer)

(define-keymap :keymap Buffer-menu-mode-map
  "<return>" #'Buffer-menu-other-window
  "<backspace>" (=> (adh--with-saved-window #'Buffer-menu-other-window))
  "i" #'Buffer-menu-this-window)

(with-eval-after-load 'vertico-multiform
  (define-keymap :keymap vertico-multiform-map
    "<left>" #'backward-char
    "<right>" #'forward-char
    "C-<return>" #'vertico-exit
    "C-f" #'vertico-multiform-vertical))

(with-eval-after-load 'completion-preview
  (define-keymap :keymap completion-preview-active-mode-map
    "<tab>" #'completion-preview-insert
    "C-<return>" #'completion-preview-insert
    "C-a" #'completion-preview-next-candidate
    "C-e" #'completion-preview-prev-candidate))

(with-eval-after-load 'corfu
  (define-keymap :keymap corfu-map
    "M-SPC" #'corfu-insert-separator
    "C-<return>" #'corfu-insert
    "C-a" #'corfu-next
    "C-e" #'corfu-previous
    "C-h" #'corfu-popupinfo-toggle))

(with-eval-after-load 'multiple-cursors-core
  (define-keymap :keymap mc/keymap
    "<return>" nil
    "C-t" #'mc/keyboard-quit
    "M-/" #'mc-hide-unmatched-lines-mode))

(with-eval-after-load 'mc-hide-unmatched-lines-mode
  (define-keymap :keymap hum/hide-unmatched-lines-mode-map
    "<return>" nil
    "C-t" #'hum/keyboard-quit))

(with-eval-after-load 'isearch
  (define-keymap :keymap isearch-mode-map
    "C-a" #'isearch-ring-advance
    "C-d" #'avy-isearch
    "C-e" #'isearch-ring-retreat
    "C-f" #'consult-isearch-history
    "C-t" #'isearch-abort
    "C-u" #'isearch-yank-kill
    "M-a" #'adh-isearch-occur
    "M-o" #'adh-isearch-mc-mark-all
    "M-t" #'isearch-query-replace
    "M-<" #'isearch-end-of-buffer
    "M->" #'isearch-beginning-of-buffer))

(with-eval-after-load 'dired
  (define-keymap :keymap dired-mode-map
    "C-t" nil
    "<backspace>" #'dired-display-file
    "<return>" #'dired-find-file-other-window
    "l" #'clipboard-kill-ring-save
    "s" #'adh-dired-sort-toggle-or-edit
    "h" #'dired-up-directory
    "i" #'dired-find-file
    "C-," #'adh-dired-duplicate-dwim
    "M-a" #'dired-toggle-read-only))

(with-eval-after-load 'dirvish
  (define-keymap :keymap dirvish-mode-map
    "TAB" #'dirvish-subtree-toggle
    "<backtab>" (cons "dirvish-subtree-close"
                      (=> (dirvish-subtree-up) (when (dirvish-subtree--expanded-p) (dirvish-subtree-toggle))))
    "," #'dirvish-layout-toggle
    "." #'dirvish-fd-search
    "g" #'dirvish-fd-search-again
    "/" #'dirvish-narrow
    "y" #'dirvish-yank-menu
    "v" #'dirvish-vc-menu))

(with-eval-after-load 'wdired
  (keymap-set wdired-mode-map "M-a" #'wdired-abort-changes))

(with-eval-after-load 'org
  (define-keymap :keymap org-mode-map
    "C-," #'adh-duplicate-dwim
    "M-h" #'previous-buffer))

(with-eval-after-load 'org-agenda
  (keymap-set org-agenda-mode-map "m" #'org-agenda-month-view))

(with-eval-after-load 'xref
  (define-keymap :keymap xref--xref-buffer-mode-map
    "<backspace>" #'xref-show-location-at-point
    "." #'xref-prev-line
    "," #'xref-next-line))

(with-eval-after-load 'compile
  (define-keymap :keymap compilation-mode-map
    "<backspace>" #'compilation-display-error
    "." #'previous-error-no-select
    "," #'next-error-no-select
    "l" #'clipboard-kill-ring-save))

(with-eval-after-load 'flycheck
  (keymap-set global-map "M-g f" 'flycheck-command-map)
  (define-keymap :keymap flycheck-error-list-mode-map
    "<backspace>" #'adh-flycheck-display-diagnostic
    "." #'flycheck-error-list-previous-error
    "," #'flycheck-error-list-next-error))

(with-eval-after-load 'elec-pair
  (keymap-set electric-pair-mode-map "<backspace>"
              '(menu-item "" electric-pair-delete-pair
                          :filter (lambda (_)
                                    (unless (use-region-p)
                                      (keymap-lookup electric-pair-mode-map "DEL"))))))

(with-eval-after-load 'grep
  (define-keymap :keymap grep-mode-map
    "<backspace>" #'compilation-display-error
    "." #'previous-error-no-select
    "," #'next-error-no-select
    "l" #'clipboard-kill-ring-save
    "M-a" #'wgrep-change-to-wgrep-mode))

(with-eval-after-load 'wgrep
  (keymap-set wgrep-mode-map "M-a" #'wgrep-abort-changes))

(with-eval-after-load 'replace
  (define-keymap :keymap occur-mode-map
    "<backspace>" #'occur-mode-display-occurrence
    "." #'previous-error-no-select
    "," #'next-error-no-select
    "l" #'clipboard-kill-ring-save
    "M-a" #'occur-edit-mode)

  (define-keymap :keymap occur-edit-mode-map
    "M-a" #'adh-occur-edit-abort
    "C-x C-s" #'adh-occur-edit-save))

(with-eval-after-load 'vundo
  (define-keymap :keymap vundo-mode-map
    "C-t" #'vundo-quit
    "h" #'vundo-backward
    "i" #'vundo-forward
    "<remap> <next-line>" #'vundo-next
    "<remap> <previous-line>" #'vundo-previous
    "<remap> <adh-mc-keyboard-quit-dwim>" #'vundo-quit))

(with-eval-after-load 'transient
  (keymap-set transient-base-map "<escape>" #'transient-quit-one)
  (keymap-set transient-map "C-t" #'transient-quit-one))

(with-eval-after-load 'rect
  (keymap-set rectangle-mark-mode-map "C-t" nil))

(with-eval-after-load 'shell
  (keymap-set shell-command-mode-map "q"
              `(menu-item "quit-window" ,(=> (quit-window t))
                          :filter ,(lambda (cmd) (unless (bound-and-true-p meow-insert-mode) cmd)))))

(with-eval-after-load 'magit
  (define-keymap :keymap magit-mode-map
    "<return>" #'adh-magit-visit-thing-other-window
    "<backspace>" #'adh-magit-preview-thing
    "," #'magit-section-forward
    "." #'magit-section-backward
    "M-1" #'magit-section-show-level-1-all
    "M-2" #'magit-section-show-level-2-all
    "M-3" #'magit-section-show-level-3-all
    "M-4" #'magit-section-show-level-4-all)

  (keymap-set magit-hunk-section-map "/" #'diff-refine-hunk)

  (define-keymap :keymap magit-mode-map
    "C-c m l" #'magit-smerge-keep-lower
    "C-c m c" #'magit-smerge-keep-current
    "C-c m b" #'magit-smerge-keep-base
    "C-c m u" #'magit-smerge-keep-upper
    "C-c m a" #'magit-smerge-keep-all)

  (define-keymap :keymap magit-blame-read-only-mode-map
    "M-<return>" #'adh-magit-show-commit-original
    "," #'magit-blame-next-chunk
    "." #'magit-blame-previous-chunk
    "w" #'magit-blame-copy-hash
    "C-w" #'adh-magit-blame-copy-short-hash))

(with-eval-after-load 'git-commit
  (keymap-set git-commit-mode-map "C-c d" #'adh-git-commit-toggle-diff))

(with-eval-after-load 'git-rebase
  (define-keymap :keymap git-rebase-mode-map
    "M-a" #'git-rebase-move-line-down
    "M-e" #'git-rebase-move-line-up))

(with-eval-after-load 'diff-mode
  (define-keymap :keymap diff-mode-read-only-map
    "," #'diff-hunk-next
    "." #'diff-hunk-prev
    "/" #'diff-refine-hunk))

(add-hook 'ediff-keymap-setup-hook
          (lambda ()
            (define-keymap :keymap ediff-mode-map
              "," #'ediff-next-difference
              "." #'ediff-previous-difference)))

(with-eval-after-load 'diff-hl
  (define-keymap :keymap diff-hl-mode-map
    "C-M-/" #'diff-hl-ediff-current-hunk
    "C-M-," #'diff-hl-next-hunk
    "C-M-." #'diff-hl-previous-hunk)

  (define-keymap :keymap diff-hl-command-map
    "," #'diff-hl-next-hunk
    "." #'diff-hl-previous-hunk
    "[" #'diff-hl-next-hunk
    "]" #'diff-hl-previous-hunk)

  (defvar-keymap adh-diff-hl-repeat-map
    :repeat t
    "," #'diff-hl-next-hunk
    "." #'diff-hl-previous-hunk
    "[" #'diff-hl-next-hunk
    "]" #'diff-hl-previous-hunk))

(with-eval-after-load 'ibuffer
  (define-keymap :keymap ibuffer-mode-map
    "C-t" nil
    "<return>" #'ibuffer-visit-buffer-other-window
    "<backspace>" #'ibuffer-visit-buffer-other-window-noselect
    "i" #'ibuffer-visit-buffer))

(provide 'adh-keybinds)

;;; adh-keybinds.el ends here
