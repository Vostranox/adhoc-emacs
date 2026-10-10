;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-functions)

(defvar meow-normal-state-keymap)
(defvar vertico-multiform-map)
(defvar magit-mode-map)
(defvar magit-hunk-section-map)
(defvar magit-blame-read-only-mode-map)
(defvar git-commit-mode-map)
(defvar ibuffer-mode-map)
(defvar ediff-mode-map
(defvar electric-pair-mode-map)

(declare-function dirvish-subtree--expanded-p "dirvish-subtree")
(declare-function ibuffer-visit-buffer "ibuffer")
(declare-function ibuffer-visit-buffer-other-window "ibuffer")
(declare-function ibuffer-visit-buffer-other-window-noselect "ibuffer")

;;; global map

(define-keymap :keymap global-map
  "<backspace>" #'adh-backward-delete-char-dwim
  "C-s" #'adh-isearch-forward-with-region
  "C-r" #'adh-isearch-backward-with-region
  "C-." #'embark-act
  "C-," #'adh-duplicate-dwim
  "C-+" #'global-text-scale-adjust
  "M-<up>" #'adh-move-lines-up
  "M-<down>" #'adh-move-lines-down
  "C-x b" #'adh-consult-buffer
  "C-x d" #'adh-switch-dired-dwim
  "C-x g" #'adh-magit-status-dwim
  "C-x u" #'vundo
  "C-x ." #'adh-settings
  "M-g f" #'consult-flycheck
  "M-g i" #'consult-imenu-multi)

(keymap-set help-map "i" #'adh-consult-info)
(keymap-set global-map "<remap> <repeat-complex-command>" #'consult-complex-command)
(keymap-set global-map "<remap> <completion-at-point>" #'adh-complete-at-point)

;;; minibuffer map

(define-keymap :keymap minibuffer-local-map
  "C-r" #'consult-history
  "C-o" (cons "consult-recent-file" (=> (adh--minibuffer-pivot #'consult-recent-file)))
  "M-d" #'adh-consult-dirs-pivot
  "M-r" (cons "adh-consult-ripgrep-project" (=> (adh--minibuffer-pivot #'adh-consult-ripgrep-project)))
  "M-s" (cons "adh-consult-fd-project" (=> (adh--minibuffer-pivot #'adh-consult-fd-project)))
  "M-o" (cons "adh-consult-zoxide" (=> (adh--minibuffer-pivot #'adh-consult-zoxide)))
  "M-a" #'embark-export
  "M-." #'adh-consult-root-pivot
  "C-x b" (cons "adh-consult-buffer" (=> (adh--minibuffer-pivot #'adh-consult-buffer)))
  "<backspace>" #'adh-backward-delete-char-dwim)

(with-eval-after-load 'consult
  (keymap-set consult-narrow-map "<backspace>" consult--narrow-delete))

(define-keymap :keymap minibuffer-local-shell-command-map
  "M-d" #'adh-shell-command-dir-pivot
  "M-." #'adh-shell-command-root-pivot)

;;; leader map

(adh-defkeymap adh-leader-map
  :map global-map
  :prefix "C-x C-o"
  "SPC" #'adh-consult-fd-project
  "," #'adh-consult-buffer
  "." #'find-file
  "RET" #'consult-bookmark
  "u" #'universal-argument
  "f" #'adh-consult-fd-project
  "F" #'adh-consult-fd-here
  "/" #'adh-consult-ripgrep-project
  "?" #'adh-consult-ripgrep-here
  "b" #'adh-consult-buffer
  "r" #'consult-recent-file
  "s" #'consult-imenu-multi
  "d" #'consult-flycheck
  "e" #'adh-switch-dired-dwim
  "E" #'adh-dired-or-file
  "z" #'adh-consult-zoxide
  "j" #'consult-bookmark
  "x" #'adh-toggle-meow-motion-mode
  "DEL" #'find-file)

(adh-defkeymap adh-git-keymap
  :map adh-leader-map
  :prefix "g"
  "g" #'adh-magit-status-dwim
  "G" #'magit-dispatch
  "F" #'magit-file-dispatch
  "e" #'adh-magit-staging-quick
  "o" #'adh-switch-magit-buffer
  "l" #'adh-git-log
  "f" #'adh-git-log-file
  "t" #'adh-git-log-line
  "s" #'adh-git-status
  "d" #'adh-git-hunks
  "b" #'adh-git-branches
  "z" #'adh-git-stash
  "/" #'magit-find-file)

(adh-defkeymap adh-window-keymap
  :map adh-leader-map
  :prefix "w"
  "s" #'split-window-vertically
  "v" #'split-window-horizontally
  "c" #'adh-delete-window
  "o" #'adh-delete-other-windows
  "k" #'kill-buffer-and-window
  "x" #'window-swap-states
  "r" #'window-layout-rotate-clockwise
  "=" #'balance-windows
  "p" #'popper-toggle
  "P" #'adh-popup-toggle-type)

(adh-defkeymap adh-buffer-keymap
  :map adh-leader-map
  :prefix "q"
  "q" #'kill-current-buffer
  "k" #'kill-buffer
  "o" #'adh-kill-other-buffers
  "m" #'adh-kill-matching-buffers-no-ask-except-current
  "r" #'rename-buffer
  "g" #'revert-buffer
  "s" #'adh-scratch-buffer
  "a" #'align-regexp
  "e" #'ediff-buffers
  "E" #'ediff-files
  "b" #'eval-buffer
  "v" #'eval-region)

(adh-defkeymap adh-compile-keymap
  :map adh-leader-map
  :prefix "c"
  "c" #'adh-project-compile-region
  "C" #'adh-compile-region
  "a" #'adh-project-async-shell-command-region
  "A" #'adh-async-shell-command-region
  "t" #'adh-tmux-send-region
  "i" #'adh-tmux-insert-pane
  "I" #'adh-tmux-insert-pane-all
  "." #'recompile)

(adh-defkeymap adh-copy-keymap
  :map adh-leader-map
  :prefix "y"
  "n" #'adh-copy-file-name
  "p" #'adh-copy-path
  "P" #'adh-copy-full-path)

(adh-defkeymap adh-tab-keymap
  :map adh-leader-map
  :prefix "t"
  "t" #'adh-tab-switch
  "c" #'tab-close
  "o" #'tab-close-other
  "r" #'tab-rename)

(adh-defkeymap adh-bookmark-keymap
  :map adh-leader-map
  :prefix "m"
  "m" #'consult-bookmark
  "s" #'bookmark-set
  "d" #'bookmark-delete
  "r" #'bookmark-rename)

(adh-defkeymap adh-replace-keymap
  :map adh-leader-map
  :prefix "%"
  "%" #'query-replace
  "r" #'vr/query-replace
  "a" #'vr/replace
  "m" #'vr/mc-mark)

(adh-defkeymap adh-lookup-keymap
  :map adh-leader-map
  :prefix "l"
  "l" #'adh-consult-locate
  "x" #'adh-get-executable
  "v" #'adh-getenv)

;;; modal mode

(with-eval-after-load 'meow
  (keymap-set global-map "C-x C-z" meow-normal-state-keymap)

  (meow-define-keys 'normal
    (cons "SPC" adh-leader-map)
    '("<escape>" . adh-mc-keyboard-quit-dwim)
    '("h" . backward-char)
    '("j" . next-line)
    '("k" . previous-line)
    '("l" . forward-char)
    '("w" . forward-word)
    '("b" . backward-word)
    '("e" . mark-word)
    '("H" . back-to-indentation)
    '("L" . end-of-visual-line)
    '("x" . adh-mark-line)
    '("X" . kill-whole-line)
    '("d" . adh-kill-region-or-line)
    '("D" . kill-word)
    '("m" . kill-sexp)
    '("M" . kill-rectangle)
    '("c" . adh-meow-insert-replace)
    '("C" . mc/mark-next-like-this)
    '("i" . adh-meow-insert)
    '("o" . adh-insert-line-below)
    '("O" . adh-insert-line-above)
    '("C-d" . adh-scroll-down-half)
    '("C-u" . adh-scroll-up-half)
    '("C-o" . pop-to-mark-command)
    '("J" . adh-join-line-above)
    '("K" . join-line)
    '("y" . clipboard-kill-ring-save)
    '("p" . clipboard-yank)
    '("P" . consult-yank-pop)
    '("u" . undo)
    '("U" . undo-redo)
    '("f" . set-mark-command)
    '("a" . exchange-point-and-mark)
    '("n" . set-mark-command)
    '("v" . rectangle-mark-mode)
    '("V" . string-insert-rectangle)
    '("r" . repeat)
    '("s" . adh-isearch-forward-with-region)
    '("S" . adh-isearch-backward-with-region)
    '("t" . adh-avy-goto-line-indent)
    '("z" . recenter-top-bottom)
    '("G" . consult-goto-line)
    '("g g" . beginning-of-buffer)
    '("g e" . end-of-buffer)
    '("g h" . beginning-of-visual-line)
    '("g l" . end-of-visual-line)
    '("g s" . back-to-indentation)
    '("g r" . xref-find-references)
    '("g n" . next-buffer)
    '("g p" . previous-buffer)
    '("g w" . adh-avy-goto-line-indent)
    '("g c" . comment-line)
    '("g x" . adh-kill-line-above)
    '("g =" . indent-region)
    '("7" . mc/unmark-previous-like-this)
    '("8" . mc/mark-next-like-this)
    '("9" . mc/mark-previous-like-this)
    '("0" . mc/unmark-next-like-this)
    '("2" . mc/skip-to-previous-like-this)
    '("3" . mc/skip-to-next-like-this)
    '("," . forward-paragraph)
    '("." . backward-paragraph)
    '("/" . mark-paragraph)
    '("<" . forward-sexp)
    '(">" . backward-sexp)
    '("(" . adh-scroll-down-half)
    '(")" . adh-scroll-up-half)
    '("[" . end-of-defun)
    '("]" . beginning-of-defun)
    '("\\" . mark-defun)
    '("-" . adh-down-list)
    '("=" . backward-up-list)
    '("_" . end-of-buffer)
    '("+" . beginning-of-buffer)
    '("{" . pop-to-mark-command)
    '("#" . adh-mark-inside)
    '("$" . mark-sexp)
    '(";" . adh-mark-inside)
    '("'" . mark-sexp)
    '(":" . comment-line)
    '("!" . set-mark-command)
    '("?" . adh-meow-insert-replace))

  (meow-define-keys 'motion
    (cons "SPC" adh-leader-map)
    '("<escape>" . adh-mc-keyboard-quit-dwim)
    '("j" . next-line)
    '("k" . previous-line)
    '("C-d" . adh-scroll-down-half)
    '("C-u" . adh-scroll-up-half)
    '("(" . adh-scroll-down-half)
    '(")" . adh-scroll-up-half)))

(define-keymap :keymap completion-in-region-mode-map
  "<return>" (adh--completions-key #'adh-completion-choose)
  "<tab>" (adh--completions-key #'adh-completion-choose)
  "C-<return>" (adh--completions-key #'adh-completion-choose)
  "C-s" (adh--completions-key #'adh-completion-in-region-isearch)
  "C-n" (adh--completions-key #'minibuffer-next-completion)
  "C-p" (adh--completions-key #'minibuffer-previous-completion))

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
    "C-<return>" #'vertico-exit))

(with-eval-after-load 'completion-preview
  (define-keymap :keymap completion-preview-active-mode-map
    "<tab>" #'completion-preview-insert
    "C-<return>" #'completion-preview-insert))

(with-eval-after-load 'corfu
  (define-keymap :keymap corfu-map
    "M-SPC" #'corfu-insert-separator
    "C-<return>" #'corfu-insert))

(with-eval-after-load 'multiple-cursors-core
  (define-keymap :keymap mc/keymap
    "<return>" nil
    "M-/" #'mc-hide-unmatched-lines-mode))

(with-eval-after-load 'mc-hide-unmatched-lines-mode
  (keymap-set hum/hide-unmatched-lines-mode-map "<return>" nil))

(with-eval-after-load 'isearch
  (define-keymap :keymap isearch-mode-map
    "C-'" #'avy-isearch
    "M-e" #'consult-isearch-history
    "M-o" #'adh-isearch-mc-mark-all))

(with-eval-after-load 'dired
  (define-keymap :keymap dired-mode-map
    "<backspace>" #'dired-display-file
    "h" #'dired-up-directory
    "l" #'dired-find-file
    "s" #'adh-dired-sort-toggle-or-edit
    "C-," #'adh-dired-duplicate-dwim))

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

(with-eval-after-load 'org
  (keymap-set org-mode-map "C-," #'adh-duplicate-dwim))

(with-eval-after-load 'xref
  (define-keymap :keymap xref--xref-buffer-mode-map
    "<backspace>" #'xref-show-location-at-point
    "." #'xref-prev-line
    "," #'xref-next-line))

(with-eval-after-load 'compile
  (define-keymap :keymap compilation-mode-map
    "<backspace>" #'compilation-display-error
    "." #'previous-error-no-select
    "," #'next-error-no-select))

(with-eval-after-load 'flycheck
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
    "," #'next-error-no-select))

(with-eval-after-load 'replace
  (define-keymap :keymap occur-mode-map
    "<backspace>" #'occur-mode-display-occurrence
    "." #'previous-error-no-select
    "," #'next-error-no-select)

  (define-keymap :keymap occur-edit-mode-map
    "C-c C-k" #'adh-occur-edit-abort
    "C-x C-s" #'adh-occur-edit-save))

(with-eval-after-load 'vundo
  (define-keymap :keymap vundo-mode-map
    "h" #'vundo-backward
    "l" #'vundo-forward
    "<remap> <next-line>" #'vundo-next
    "<remap> <previous-line>" #'vundo-previous
    "<remap> <adh-mc-keyboard-quit-dwim>" #'vundo-quit))

(with-eval-after-load 'transient
  (keymap-set transient-base-map "<escape>" #'transient-quit-one))

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
    "<return>" #'ibuffer-visit-buffer-other-window
    "<backspace>" #'ibuffer-visit-buffer-other-window-noselect
    "i" #'ibuffer-visit-buffer))

(provide 'adh-keybinds-qwerty)

;;; adh-keybinds-qwerty.el ends here
