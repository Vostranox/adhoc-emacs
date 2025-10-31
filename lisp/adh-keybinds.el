;;; -*- lexical-binding: t; coding: utf-8 -*-

(defmacro => (&rest body)
  `(lambda () (interactive) ,@body))

;; full global override (unused)
(defvar adh-global-map (make-sparse-keymap))
(unless (member `((t . ,adh-global-map)) emulation-mode-map-alists)
  (add-to-list 'emulation-mode-map-alists `((t . ,adh-global-map))))

;; global-map
(keymap-set global-map "<backspace>" #'adh-backward-delete-char-dwim)

(keymap-set global-map "C-f" #'completion-at-point)
(keymap-set global-map "C-o" #'zoxide-travel)
(keymap-set global-map "C-u" #'clipboard-yank)
(keymap-set global-map "C-S-u" #'consult-yank-pop)

(keymap-set global-map "C-h" #'mark-word)
(keymap-set global-map "C-S-h" #'mark-sexp)
(keymap-set global-map "C-a" #'next-line)
(keymap-set global-map "C-e" #'previous-line)
(keymap-set global-map "C-i" #'indent-for-tab-command)
(keymap-set global-map "C-S-i" #'tab-to-tab-stop)

(keymap-set global-map "C-," #'adh-duplicate-dwim)
(keymap-set global-map "C-." #'embark-act)

(keymap-set global-map "C-l" #'recenter-top-bottom)
(keymap-set global-map "C-d" #'other-window)
(keymap-set global-map "C-b" #'adh-select-side-window)

(keymap-set global-map "C-n" #'consult-isearch-history)
(keymap-set global-map "C-r" #'adh-isearch-backward-with-region)
(keymap-set global-map "C-t" #'adh-keyboard-quit-dwim)
(keymap-set global-map "C-s" #'adh-isearch-forward-with-region)
(keymap-set global-map "C-g" #'adh-keyboard-quit-dwim)

(keymap-set global-map "C-+" #'global-text-scale-adjust)

(keymap-set global-map "M-f" #'forward-word)
(keymap-set global-map "M-o" #'adh-project-switch-to-dired)
(keymap-set global-map "M-u" #'indent-region)

(keymap-set global-map "M-h" #'previous-buffer)
(keymap-set global-map "M-a" #'adh-move-lines-down)
(keymap-set global-map "M-e" #'adh-move-lines-up)
(keymap-set global-map "M-i" #'next-buffer)

(keymap-set global-map "M-," #'xref-go-back)
(keymap-set global-map "M-." #'xref-find-definitions)
(keymap-set global-map "M-/" #'xref-find-references)

(keymap-set global-map "M-l" #'clipboard-kill-ring-save)
(keymap-set global-map "M-d" #'kill-word)
(keymap-set global-map "M-b" #'backward-word)

(keymap-set global-map "M-n" #'set-mark-command)
(keymap-set global-map "M-r" #'consult-register-store)
(keymap-set global-map "M-t" #'consult-register-load)
(keymap-set global-map "M-s" #'avy-goto-char-timer)

(keymap-set global-map "M-m" #'transpose-chars)
(keymap-set global-map "M-w" #'transpose-words)

(keymap-set global-map "M-<delete>" #'backward-kill-word)

(keymap-set global-map "C-M-f" #'downcase-dwim)
(keymap-set global-map "C-M-o" #'capitalize-dwim)
(keymap-set global-map "C-M-u" #'upcase-dwim)
(keymap-set global-map "C-M-s" #'vr/isearch-forward)
(keymap-set global-map "C-M-r" #'vr/isearch-backward)

;; minibuffer-local-map
(keymap-set minibuffer-local-map "<backspace>" #'adh-backward-delete-char-dwim)
(keymap-set minibuffer-local-map "C-u" #'clipboard-yank)
(keymap-set minibuffer-local-map "C-h" #'mark-word)
(keymap-set minibuffer-local-map "C-r" #'consult-history)
(keymap-set minibuffer-local-map "M-a" #'embark-export)

;; minibuffer-local-shell-command-map
(keymap-set minibuffer-local-shell-command-map "C-e" #'previous-history-element)
(keymap-set minibuffer-local-shell-command-map "C-a" #'adh-minibuffer-next-history-or-clear)

;; adh-command-map
(defvar-keymap adh-command-map)
(keymap-set global-map "C-c" adh-command-map)

(keymap-set adh-command-map "f" #'mc/edit-beginnings-of-lines)
(keymap-set adh-command-map "o" #'mc/mark-all-dwim)
(keymap-set adh-command-map "u" #'mc/insert-numbers)

(keymap-set adh-command-map "a" #'adh-tmux-cd)
(keymap-set adh-command-map "r" #'adh-wrap-region-with-pair)
(keymap-set adh-command-map "C-u" #'vundo)
(keymap-set adh-command-map "C-h" #'help-command)

;; adh-command2-map
(defvar-keymap adh-command2-map)
(keymap-set global-map "M-c" adh-command2-map)

;; C-x map
(keymap-set global-map "C-x j" (=> (find-file (adh--get-project-dir))))
(keymap-set global-map "C-x C-j"  #'dired-jump)
(keymap-set global-map "C-x f" #'find-file-at-point)
(keymap-set global-map "C-x C-f" #'find-file)
(keymap-set global-map "C-x RET f" #'set-buffer-file-coding-system)
(keymap-set global-map "C-x RET o" #'adh-show-buffer-file-encoding)
(keymap-set global-map "C-x -" #'shrink-window-if-larger-than-buffer)
(keymap-set global-map "C-x n d" #'narrow-to-defun)
(keymap-set global-map "C-x n n" #'narrow-to-region)
(keymap-set global-map "C-x n w" #'widen)
(keymap-set global-map "C-x ," #'next-error)
(keymap-set global-map "C-x ." #'previous-error)

;; leader map
(defvar-keymap adh-leader-map)
(keymap-set global-map "C-x a" adh-leader-map)

;; adh-find-keymap
(defvar-keymap adh-find-keymap)
;; adh-find-keymap keys
(keymap-set adh-find-keymap "f" (=> (adh--consult-fd-with-region default-directory)))
(keymap-set adh-find-keymap "o" (=> (adh-consult-fd-directories default-directory)))
(keymap-set adh-find-keymap "h" (=> (adh--consult-fd-with-region (adh--get-project-dir))))
(keymap-set adh-find-keymap "a" (=> (adh-consult-fd-directories (adh--get-project-dir))))
(keymap-set adh-find-keymap "e" (=> (adh--consult-fd-with-region 1)))
(keymap-set adh-find-keymap "p" #'consult-locate)
;; adh-find-keymap to adh-leader-map
(keymap-set adh-leader-map "s" adh-find-keymap)

;; adh-search-keymap
(defvar-keymap adh-search-keymap)
;; adh-search-keymap keys
(keymap-set adh-search-keymap "f" #'adh-consult-ripgrep-here)
(keymap-set adh-search-keymap "h" #'adh-consult-ripgrep-project)
(keymap-set adh-search-keymap "e" #'adh-consult-ripgrep-all)
(keymap-set adh-search-keymap "o" #'adh-consult-imenu-any)
(keymap-set adh-search-keymap "a" #'adh-consult-line-with-region)
;; adh-search-keymap to adh-leader-map
(keymap-set adh-leader-map "t" adh-search-keymap)

;; adh-replace-keymap
(defvar-keymap adh-replace-keymap)
;; adh-replace-keymap keys
(keymap-set adh-replace-keymap "f" #'query-replace)
(keymap-set adh-replace-keymap "h" #'vr/query-replace)
(keymap-set adh-replace-keymap "a" #'vr/replace)
(keymap-set adh-replace-keymap "e" #'vr/mc-mark)
;; adh-replace-keymap to adh-leader-map
(keymap-set adh-leader-map "r" adh-replace-keymap)

;; adh-compile-keymap
(defvar-keymap adh-compile-keymap)
;; adh-compile-keymap keys
(keymap-set adh-compile-keymap "f" #'compile)
(keymap-set adh-compile-keymap "o" #'async-shell-command)
(keymap-set adh-compile-keymap "u" #'adh-compile-region)
(keymap-set adh-compile-keymap "h" #'adh-project-compile)
(keymap-set adh-compile-keymap "a" #'adh-project-async-shell-command)
(keymap-set adh-compile-keymap "e" #'recompile)
(keymap-set adh-compile-keymap "." #'adh-project-compile-region)
;; adh-compile-keymap to adh-leader-map
(keymap-set adh-leader-map "n" adh-compile-keymap)

;; adh-magit-keymap
(defvar-keymap adh-magit-keymap)
;; adh-magit-keymap keys
(keymap-set adh-magit-keymap "c" #'adh-magit-switch-or-status)
(keymap-set adh-magit-keymap "m" #'magit)
(keymap-set adh-magit-keymap "j" #'magit-file-dispatch)
(keymap-set adh-magit-keymap "f" #'adh-toggle-magit-blame)
(keymap-set adh-magit-keymap "o" #'magit-log-buffer-file)
(keymap-set adh-magit-keymap "u" #'magit-ediff-show-commit)
(keymap-set adh-magit-keymap "y" #'magit-dispatch)
(keymap-set adh-magit-keymap "h" #'adh-magit-staging-quick)
(keymap-set adh-magit-keymap "a" #'magit-log-current)
(keymap-set adh-magit-keymap "e" #'magit-checkout)
(keymap-set adh-magit-keymap "." #'magit-status-quick)
(keymap-set adh-magit-keymap "," #'magit-git-command-topdir)
(keymap-set adh-magit-keymap "/" #'adh-magit-restore-current)
;; adh-magit-keymap to adh-leader-map
(keymap-set adh-leader-map "m" adh-magit-keymap)

;; adh-file-keymap
(defvar-keymap adh-file-keymap)
;; adh-file-keymap keys
(keymap-set adh-file-keymap "d" #'rename-file)
(keymap-set adh-file-keymap "c" #'write-file)
(keymap-set adh-file-keymap "s" #'adh-copy-full-path)
(keymap-set adh-file-keymap "t" #'adh-copy-path)
(keymap-set adh-file-keymap "r" #'adh-copy-file-name)
(keymap-set adh-file-keymap "x" #'ediff-files)
;; adh-file-keymap to adh-leader-map
(keymap-set adh-leader-map "f" adh-file-keymap)

;; adh-toggle-keymap
(defvar-keymap adh-toggle-keymap)
;; adh-toggle-keymap keys
(keymap-set adh-toggle-keymap "l" #'adh-toggle-vc-mode)
(keymap-set adh-toggle-keymap "d" #'flymake-show-buffer-diagnostics)
(keymap-set adh-toggle-keymap "c" #'adh-toggle-corfu-auto)
(keymap-set adh-toggle-keymap "r" #'adh-toggle-eglot-flymake)
(keymap-set adh-toggle-keymap "t" #'adh-toggle-eglot-global)
(keymap-set adh-toggle-keymap "s" #'adh-toggle-eglot-format-on-save)
;; adh-toggle-keymap to adh-leader-map
(keymap-set adh-leader-map "u" adh-toggle-keymap)

;; adh-window-keymap
(defvar-keymap adh-window-keymap)
;; adh-window-keymap keys
(keymap-set adh-window-keymap "l" #'kill-buffer-and-window)
(keymap-set adh-window-keymap "d" #'windower-toggle-single)
(keymap-set adh-window-keymap "c" #'delete-window)
(keymap-set adh-window-keymap "b" #'delete-other-windows-vertically)
(keymap-set adh-window-keymap "n" #'balance-windows)
(keymap-set adh-window-keymap "r" #'windower-toggle-split)
(keymap-set adh-window-keymap "t" #'split-window-vertically)
(keymap-set adh-window-keymap "s" #'split-window-horizontally)
(keymap-set adh-window-keymap "w" #'windower-swap)
(keymap-set adh-window-keymap "m" #'window-toggle-side-windows)
(keymap-set adh-window-keymap "x" #'adh-to-side-window)
;; adh-window-keymap to adh-leader-map
(keymap-set adh-leader-map "h" adh-window-keymap)

;; adh-buffer-keymap
(defvar-keymap adh-buffer-keymap)
;; adh-buffer-keymap keys
(keymap-set adh-buffer-keymap "l" #'persp-kill-buffer*)
(keymap-set adh-buffer-keymap "d" #'adh-kill-persp-other-buffers)
(keymap-set adh-buffer-keymap "c" #'kill-current-buffer)
(keymap-set adh-buffer-keymap "b" #'adh-kill-matching-buffers-no-ask-except-current)
(keymap-set adh-buffer-keymap "n" #'align-regexp)
(keymap-set adh-buffer-keymap "r" #'rename-buffer)
(keymap-set adh-buffer-keymap "t" #'previous-buffer)
(keymap-set adh-buffer-keymap "s" #'next-buffer)
(keymap-set adh-buffer-keymap "g" #'revert-buffer)
(keymap-set adh-buffer-keymap "x" #'ediff-buffers)
(keymap-set adh-buffer-keymap "m" #'eval-buffer)
(keymap-set adh-buffer-keymap "w" #'eval-region)
(keymap-set adh-buffer-keymap "h" #'ibuffer)
;; adh-buffer-keymap to adh-leader-map
(keymap-set adh-leader-map "a" adh-buffer-keymap)

;; adh-persp-keymap
(defvar-keymap adh-persp-keymap)
;; adh-persp-keymap keys
(keymap-set adh-persp-keymap "l" #'persp-switch-to-scratch-buffer)
(keymap-set adh-persp-keymap "d" #'persp-kill-others)
(keymap-set adh-persp-keymap "c" #'persp-kill)
(keymap-set adh-persp-keymap "r" #'persp-rename)
(keymap-set adh-persp-keymap "t" #'persp-prev)
(keymap-set adh-persp-keymap "s" #'persp-next)
(keymap-set adh-persp-keymap "n" #'adh-persp-toggle-buffer-narrow)
;; adh-persp-keymap to adh-leader-map
(keymap-set adh-leader-map "e" adh-persp-keymap)

;; adh-bookmark-keymap
(defvar-keymap adh-bookmark-keymap)
;; adh-bookmark-keymap keys
(keymap-set adh-bookmark-keymap "c" #'bookmark-delete)
(keymap-set adh-bookmark-keymap "r" #'bookmark-rename)
(keymap-set adh-bookmark-keymap "s" #'bookmark-set)
;; adh-bookmark-keymap to adh-leader-map
(keymap-set adh-leader-map "i" adh-bookmark-keymap)

;; adh-leader-map
(defvar-keymap adh-leader-map)
;; adh-leader-map keys
(keymap-set adh-leader-map "l" #'persp-switch)
(keymap-set adh-leader-map "d" #'adh-consult-select-window)
(keymap-set adh-leader-map "c" #'consult-buffer)
(keymap-set adh-leader-map "b" #'consult-bookmark)
(keymap-set adh-leader-map "x" #'adh-toggle-meow-normal-mode)
(keymap-set adh-leader-map "w" #'adh-dired-or-file)
(keymap-set adh-leader-map "o" #'adh-select-side-window)
(keymap-set adh-leader-map "DEL" #'find-file)

;; Meow
(with-eval-after-load 'meow
  ;; Modal mode in the minibuffer
  (defun adh--meow-minibuffer-update-cursor ()
    (setq cursor-type
          (cond
           ((meow-normal-mode-p) 'box)
           ((meow-insert-mode-p) 'bar)
           ((meow-motion-mode-p) 'box)
           (t 'box))))

  (defun adh--meow-minibuffer-setup ()
    (meow-insert-mode 1)
    (setq-local cursor-type 'bar)
    (add-hook 'post-command-hook #'adh--meow-minibuffer-update-cursor nil t)
    (redisplay)
    (local-set-key (kbd "<escape>") #'meow-normal-mode)
    (local-set-key (kbd "C-t") #'adh-keyboard-quit-dwim))

  (add-hook 'minibuffer-setup-hook #'adh--meow-minibuffer-setup)

  ;; Normal mode
  (keymap-set global-map "C-x t" meow-normal-state-keymap)

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
    '("U" . consult-yank-pop)
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
    (cons "g h" (=> (avy-goto-line) (back-to-indentation)))
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

;; Vertico
(with-eval-after-load 'vertico
  (keymap-set vertico-multiform-map "C-f" #'vertico-multiform-vertical)
  (keymap-set vertico-multiform-map "C-e" #'vertico-previous)
  (keymap-set vertico-multiform-map "C-a" #'vertico-next)
  (keymap-set vertico-multiform-map "C-<return>" #'vertico-exit)
  (keymap-set vertico-multiform-map "<left>" #'backward-char)
  (keymap-set vertico-multiform-map "<right>" #'forward-char))

(with-eval-after-load 'vertico-reverse
  (keymap-set vertico-reverse-map "C-a" #'vertico-previous)
  (keymap-set vertico-reverse-map "C-e" #'vertico-next))

;; Corfu
(with-eval-after-load 'corfu
  (keymap-set corfu-map "C-h" #'corfu-info-documentation)
  (keymap-set corfu-map "C-a" #'corfu-next)
  (keymap-set corfu-map "C-e" #'corfu-previous)
  (keymap-set corfu-map "C-SPC" #'corfu-insert-separator)
  (keymap-set corfu-map "<return>" #'corfu-insert)
  (keymap-set corfu-map "<tab>" #'corfu-complete))

;; Consult
(with-eval-after-load 'consult
  (keymap-set consult-narrow-map "C-o" #'adh-consult-to-all))

;; Multiple-cursors
(with-eval-after-load 'multiple-cursors
  (keymap-set mc/keymap "C-t" #'mc/keyboard-quit)
  (keymap-set mc/keymap "C-g" #'mc/keyboard-quit)
  (keymap-set mc/keymap "<return>" nil)
  (keymap-set mc/keymap "<escape>" #'adh-mc-keyboard-quit-dwim))

;; Isearch
(with-eval-after-load 'isearch
  (keymap-set isearch-mode-map "C-d" #'avy-isearch)
  (keymap-set isearch-mode-map "C-t" #'isearch-abort)
  (keymap-set isearch-mode-map "C-u" #'isearch-yank-x-selection))

;; Dired
(with-eval-after-load 'dired
  (keymap-set dired-mode-map "l" #'clipboard-kill-ring-save)
  (keymap-set dired-mode-map "h" #'dired-up-directory)
  (keymap-set dired-mode-map "i" #'dired-find-file)
  (keymap-set dired-mode-map "C-o" #'zoxide-travel)
  (keymap-set dired-mode-map "C-," #'adh-dired-duplicate-file)
  (keymap-set dired-mode-map "M-a" #'dired-toggle-read-only)
  (keymap-set dired-mode-map "<backspace>" #'dired-display-file)
  (keymap-set dired-mode-map "<return>" #'dired-find-file-other-window))

;; Wdired
(with-eval-after-load 'wdired
  (keymap-set wdired-mode-map "M-a" #'wdired-abort-changes))

;; Org
(with-eval-after-load 'org
  (keymap-set org-mode-map "C-," #'adh-duplicate-dwim))

(with-eval-after-load 'xref
  (keymap-set xref--xref-buffer-mode-map "<backspace>" #'xref-show-location-at-point))

;; Compile
(with-eval-after-load 'compile
  (keymap-set compilation-mode-map "." #'previous-error-no-select)
  (keymap-set compilation-mode-map "," #'next-error-no-select)
  (keymap-set compilation-button-map "<backspace>" #'adh-compile-goto-error-and-pop))

;; Flymake
(with-eval-after-load 'flymake
  (keymap-set flymake-diagnostics-buffer-mode-map "<backspace>" #'adh-flymake-preview))

;; Grep
(with-eval-after-load 'grep
  (keymap-set grep-mode-map "." #'previous-error-no-select)
  (keymap-set grep-mode-map "," #'next-error-no-select)
  (keymap-set grep-mode-map "M-a" #'wgrep-change-to-wgrep-mode))

;; Wgrep
(with-eval-after-load 'wgrep
  (keymap-set wgrep-mode-map "M-a" #'wgrep-abort-changes))

;; Occur
(with-eval-after-load 'replace
  (keymap-set occur-mode-map "." #'previous-error-no-select)
  (keymap-set occur-mode-map "," #'next-error-no-select)
  (keymap-set occur-mode-map "<backspace>" #'adh-occur-goto-and-pop)
  (keymap-set occur-mode-map "M-a" #'occur-edit-mode)
  (keymap-set occur-edit-mode-map "M-a" #'occur-cease-edit))

;; Vundo
(with-eval-after-load 'vundo
  (keymap-set vundo-mode-map "C-t" #'vundo-quit))

;; Transient
(with-eval-after-load 'transient
  (keymap-set transient-base-map "<escape>" #'transient-quit-one))

;; Shell command
(with-eval-after-load 'shell
  (keymap-set shell-command-mode-map "q" (=> (quit-window t))))

;; Magit
(with-eval-after-load 'magit
  ;; magit-mode
  (keymap-set magit-mode-map "<enter>" #'magit-diff-visit-file-other-window)
  (keymap-set magit-mode-map "<backspace>" #'adh-magit-diff-preview-file)
  (keymap-set magit-mode-map "C-e" #'magit-previous-line)
  (keymap-set magit-mode-map "C-a" #'magit-next-line)
  (keymap-set magit-mode-map "M-<return>" #'magit-diff-toggle-refine-hunk)
  (keymap-set magit-mode-map "," #'magit-section-forward)
  (keymap-set magit-mode-map "." #'magit-section-backward)
  ;; magit-smerge
  (keymap-set magit-mode-map "C-c m u" #'magit-smerge-keep-upper)
  (keymap-set magit-mode-map "C-c m l" #'magit-smerge-keep-lower)
  (keymap-set magit-mode-map "C-c m b" #'magit-smerge-keep-base)
  (keymap-set magit-mode-map "C-c m a" #'magit-smerge-keep-all)
  (keymap-set magit-mode-map "C-c m c" #'magit-smerge-keep-current)
  ;; magit-blame
  (keymap-set magit-blame-mode-map "," #'magit-blame-next-chunk)
  (keymap-set magit-blame-mode-map "." #'magit-blame-previous-chunk)
  (keymap-set magit-blame-mode-map "w" #'magit-blame-copy-hash)
  (keymap-set magit-blame-mode-map "C-w" #'adh-magit-blame-copy-short-hash)
  (keymap-set magit-blame-mode-map "M-<return>" #'adh-magit-show-commit-original))

;; Git rebase
(with-eval-after-load 'git-rebase
  (keymap-set git-rebase-mode-map "M-a" #'git-rebase-move-line-down)
  (keymap-set git-rebase-mode-map "M-e" #'git-rebase-move-line-up))

;; nXml
(with-eval-after-load 'nxml-mode
  (setq nxml-mode-map (make-sparse-keymap))
  (set-keymap-parent nxml-mode-map text-mode-map))

(provide 'adh-keybinds)
