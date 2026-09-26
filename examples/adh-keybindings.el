;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-meow)

;;; global map

(keymap-set global-map "C-c w" #'whitespace-mode)
(keymap-set global-map "C-c d" (cons "insert-date" (=> (insert (format-time-string "%F")))))

;;; override map

(keymap-set adh-override-map "C-;" #'avy-goto-char-timer)

;;; leader map

(adh-defkeymap adh-leader-map
  :map global-map
  :prefix "C-x C-o")

(adh-defkeymap adh-org-keymap
  :map adh-leader-map
  :prefix "o"
  "a" #'org-agenda
  "c" #'org-capture
  "l" #'org-store-link)

;;; modal mode

(with-eval-after-load 'meow
  (keymap-set global-map "C-x C-z" meow-normal-state-keymap)

  (meow-define-keys 'normal
    (cons "SPC" adh-leader-map)
    '("<escape>" . adh-mc-keyboard-quit-dwim)

    '("y" . beginning-of-visual-line)
    '("u" . back-to-indentation)
    '("i" . end-of-visual-line)
    '("o" . clipboard-yank)
    '("O" . consult-yank-pop)

    '("j" . backward-char)
    '("k" . next-line)
    '("l" . previous-line)
    '(";" . forward-char)
    '("J" . join-line)
    '("K" . adh-insert-line-below)
    '("L" . adh-insert-line-above)
    '(":" . adh-join-line-above)

    '("n" . adh-mark-line)
    '("m" . mark-word)
    '("." . backward-paragraph)
    '("," . forward-paragraph)
    '("/" . mark-paragraph)

    '("w" . clipboard-kill-ring-save)
    '("e" . backward-word)
    '("r" . forward-word)
    '("t" . undo)
    '("T" . undo-redo)

    '("a" . set-mark-command)
    '("s" . exchange-point-and-mark)
    '("d" . adh-meow-insert)
    '("f" . adh-meow-insert-replace)
    '("g g" . consult-goto-line)
    '("g h" . adh-avy-goto-line-indent)

    '("z" . kill-word)
    '("x" . kill-whole-line)
    '("v" . adh-kill-region-or-line)
    '("b" . rectangle-mark-mode)
    '("B" . string-insert-rectangle))

  (meow-define-keys 'motion
    (cons "SPC" adh-leader-map)
    '("<escape>" . adh-mc-keyboard-quit-dwim)
    '("j" . next-line)
    '("k" . previous-line)))
