;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'cl-lib)
(require 'seq)

(defgroup adh-jump nil
  "Back and forward through navigation in each editing window."
  :group 'adhoc
  :prefix "adh-jump-")

(defcustom adh-jump-history-length 100
  "Maximum number of positions kept in a window's navigation history.
Zero discards positions instead of retaining them."
  :type 'natnum
  :group 'adh-jump)

(defun adh--jump-set-commands (symbol value)
  "Set SYMBOL to VALUE and update navigation command advice."
  (set-default-toplevel-value symbol value)
  (when (fboundp 'adh--jump-install)
    (adh--jump-install)))

(defcustom adh-jump-commands
  '(backward-up-list up-list down-list forward-sexp backward-sexp
    adh-forward-sexp adh-backward-sexp adh-down-list adh-match-jump
    beginning-of-buffer end-of-buffer beginning-of-defun end-of-defun
    forward-paragraph backward-paragraph goto-line
    adh-scroll-up-half adh-scroll-down-half scroll-up-command scroll-down-command
    avy-goto-char avy-goto-char-2 avy-goto-char-timer avy-goto-word-1
    avy-goto-line adh-avy-goto-line-indent
    xref-find-definitions xref-find-definitions-other-window
    xref-find-references xref-go-back xref-go-forward
    imenu consult-imenu consult-imenu-multi consult-goto-line
    consult-line consult-line-multi consult-outline consult-mark consult-global-mark
    consult-xref consult-flycheck consult-flymake consult-compile-error
    consult-ripgrep consult-grep consult-git-grep
    adh-consult-ripgrep-here adh-consult-ripgrep-project adh-consult-ripgrep-dirs
    find-file find-file-at-point project-find-file
    consult-fd consult-find consult-locate consult-recent-file
    adh-consult-fd-here adh-consult-fd-project adh-consult-fd-dirs adh-consult-locate
    switch-to-buffer next-buffer previous-buffer
    consult-buffer consult-project-buffer adh-consult-buffer
    bookmark-jump consult-bookmark jump-to-register consult-register consult-register-load
    consult-zoxide adh-switch-dired-dwim dirvish-history-jump adh-consult-info)
  "Commands whose interactive navigation records a return position.
Only the final destination is recorded, after the command returns normally.
Nested calls, minibuffer commands and picker previews do not add positions.
Incremental search is recorded separately when the search finishes."
  :type '(repeat function)
  :group 'adh-jump
  :initialize #'custom-initialize-default
  :set #'adh--jump-set-commands)

(defconst adh--jump-structural-commands
  '(backward-up-list up-list down-list forward-sexp backward-sexp
    adh-forward-sexp adh-backward-sexp adh-down-list adh-match-jump)
  "Navigation commands that restore their origin on error or quit.")

(cl-defstruct (adh--jump-history (:constructor adh--jump-history-create))
  back forward)

(defvar adh--jump-histories (make-hash-table :test #'eq :weakness 'key)
  "Window histories, containing owned markers in both directions.
New windows start with an independent, empty history.")
(defvar adh--jump-recording nil
  "Non-nil inside a navigation transaction or a history jump.")
(defvar adh--jump-advised nil
  "Commands currently carrying the navigation advice.")
(defvar adh--jump-isearch-origin nil
  "Origin of the current incremental search, including suspended searches.")

(defun adh--jump-release (positions)
  "Detach the owned markers in POSITIONS."
  (dolist (marker positions) (set-marker marker nil)))

(defun adh--jump-same-p (a b)
  "Non-nil when live markers A and B refer to the same position."
  (and (marker-buffer a) (marker-buffer b)
       (eq (marker-buffer a) (marker-buffer b))
       (= (marker-position a) (marker-position b))))

(defun adh--jump-trim (positions)
  "Bound POSITIONS and detach any evicted markers."
  (if (<= adh-jump-history-length 0)
      (progn (adh--jump-release positions) nil)
    (let ((tail (nthcdr (1- adh-jump-history-length) positions)))
      (when tail
        (adh--jump-release (cdr tail))
        (setcdr tail nil)))
    positions))

(defun adh--jump-push (marker positions)
  "Add owned MARKER to POSITIONS, merging consecutive duplicate locations."
  (if (and positions (adh--jump-same-p marker (car positions)))
      (progn (set-marker marker nil) positions)
    (adh--jump-trim (cons marker positions))))

(defun adh--jump-editing-p ()
  "Non-nil while an ordinary editing window is selected."
  (and (not (minibufferp))
       (not (window-minibuffer-p))
       (not (frame-parent))
       (eq (current-buffer) (window-buffer))
       (not (bound-and-true-p mc--executing-command-for-fake-cursor))))

(defun adh--jump-origin ()
  "Capture the current window, position, view and modification tick."
  (list (selected-window) (point-marker) (window-start) (window-hscroll)
        (buffer-chars-modified-tick)))

(defun adh--jump-commit (origin)
  "Record ORIGIN if the selected editing window reached a new location.
Record in the destination window, so Back works where a cross-window jump
lands.  Insertion into the original buffer is not a navigation event."
  (pcase-let ((`(,_ ,marker ,_ ,_ ,tick) origin))
    (when (and (marker-buffer marker) (adh--jump-editing-p)
               (or (not (eq (marker-buffer marker) (current-buffer)))
                   (and (= tick (buffer-chars-modified-tick))
                        (/= marker (point)))))
      (let* ((window (selected-window))
             (history (or (gethash window adh--jump-histories)
                          (puthash window (adh--jump-history-create) adh--jump-histories))))
        (adh--jump-release (adh--jump-history-forward history))
        (setf (adh--jump-history-forward history) nil
              (adh--jump-history-back history)
              (adh--jump-push (copy-marker marker) (adh--jump-history-back history)))))))

(defun adh--jump-goto (marker window)
  "Visit live MARKER in WINDOW, widening only if its position is hidden."
  (let ((buffer (marker-buffer marker)))
    (unless (and buffer (window-live-p window))
      (user-error "Jump location is no longer available"))
    (select-window window)
    (unless (eq buffer (current-buffer)) (switch-to-buffer buffer nil t))
    (unless (<= (point-min) marker (point-max)) (widen))
    (goto-char marker)))

(defun adh--jump-restore (origin)
  "Restore the location and view in ORIGIN after a failed structural move."
  (pcase-let ((`(,window ,marker ,start ,hscroll ,_) origin))
    (when (and (window-live-p window) (marker-buffer marker))
      (adh--jump-goto marker window)
      (set-window-start window start t)
      (set-window-hscroll window hscroll))))

(defun adh--jump-run (function &optional restore)
  "Call FUNCTION as one navigation transaction.
With RESTORE, restore the origin on an error, quit or other nonlocal exit.
The original return value or error is preserved."
  (let ((origin (adh--jump-origin))
        (adh--jump-recording t)
        completed)
    (unwind-protect
        (prog1 (funcall function) (setq completed t))
      (unwind-protect
          (if completed
              (adh--jump-commit origin)
            (when restore
              (let ((inhibit-quit t))
                (adh--jump-restore origin))))
        (set-marker (cadr origin) nil)))))

(defun adh--jump-step (forward)
  "Move one position FORWARD in history, or backward when nil."
  (let* ((window (selected-window))
         (history (gethash window adh--jump-histories))
         (positions (and history (if forward (adh--jump-history-forward history)
                                   (adh--jump-history-back history))))
         (current (point-marker))
         (adh--jump-recording t))
    (unwind-protect
        (progn
          (while (and positions
                      (or (not (marker-buffer (car positions)))
                          (adh--jump-same-p current (car positions))))
            (set-marker (pop positions) nil))
          (when history
            (if forward (setf (adh--jump-history-forward history) positions)
              (setf (adh--jump-history-back history) positions)))
          (unless positions
            (user-error "No %s jump" (if forward "later" "earlier")))
          (adh--jump-goto (car positions) window)
          (set-marker (car positions) nil)
          (if forward
              (setf (adh--jump-history-forward history) (cdr positions)
                    (adh--jump-history-back history)
                    (adh--jump-push current (adh--jump-history-back history)))
            (setf (adh--jump-history-back history) (cdr positions)
                  (adh--jump-history-forward history)
                  (adh--jump-push current (adh--jump-history-forward history))))
          (setq current nil))
      (when current (set-marker current nil)))))

(defun adh-jump-backward (&optional count)
  "Return through COUNT navigation locations in this window."
  (interactive "p")
  (dotimes (_ (abs (or count 1))) (adh--jump-step (< (or count 1) 0))))

(defun adh-jump-forward (&optional count)
  "Advance through COUNT navigation locations in this window."
  (interactive "p")
  (dotimes (_ (abs (or count 1))) (adh--jump-step (>= (or count 1) 0))))

(defun adh-jump-clear (&optional all)
  "Clear this window's navigation history; with prefix ALL, clear every window."
  (interactive "P")
  (maphash
   (lambda (window history)
     (when (or all (eq window (selected-window)))
       (adh--jump-release (adh--jump-history-back history))
       (adh--jump-release (adh--jump-history-forward history))
       (remhash window adh--jump-histories)))
   adh--jump-histories))

(defun adh--jump-prune-windows ()
  "Release histories belonging to deleted windows."
  (maphash
   (lambda (window history)
     (unless (window-live-p window)
       (adh--jump-release (adh--jump-history-back history))
       (adh--jump-release (adh--jump-history-forward history))
       (remhash window adh--jump-histories)))
   adh--jump-histories))

(defun adh--jump-isearch-start ()
  "Remember the origin of a search, retaining it across suspended searches."
  (when (and (not adh--jump-recording) (adh--jump-editing-p)
             (not adh--jump-isearch-origin))
    (setq adh--jump-isearch-origin (adh--jump-origin))))

(defun adh--jump-isearch-end ()
  "Record a completed search, ignoring cancellation and temporary suspension."
  (when (and adh--jump-isearch-origin (not (bound-and-true-p isearch-suspended)))
    (let ((origin adh--jump-isearch-origin))
      (setq adh--jump-isearch-origin nil)
      (unwind-protect
          (unless (bound-and-true-p isearch-mode-end-hook-quit)
            (adh--jump-commit origin))
        (set-marker (cadr origin) nil)))))

(defun adh--jump-kill-buffer ()
  "Release a pending search origin when its buffer is killed."
  (when (and adh--jump-isearch-origin
             (eq (marker-buffer (cadr adh--jump-isearch-origin)) (current-buffer)))
    (set-marker (cadr adh--jump-isearch-origin) nil)
    (setq adh--jump-isearch-origin nil)))

(defun adh--jump-pivot-args (args)
  "Wrap a deferred minibuffer pivot callback in ARGS as a navigation command."
  (let ((function (car args)))
    (list (lambda ()
            (if (and (not adh--jump-recording) (adh--jump-editing-p))
                (adh--jump-run function)
              (funcall function))))))

(defun adh--jump-install ()
  "Install default navigation recording, with no minor mode or mode line entry."
  (dolist (command adh--jump-advised) (advice-remove command 'adh-jump))
  (setq adh--jump-advised (copy-sequence adh-jump-commands))
  (dolist (command adh--jump-advised)
    (advice-add
     command :around
     (lambda (function &rest args)
       (let ((interactive (called-interactively-p 'any)))
         (cond
          ((and interactive (not adh--jump-recording) (adh--jump-editing-p))
           (adh--jump-run (lambda () (apply #'funcall-interactively function args))
                          (memq command adh--jump-structural-commands)))
          (interactive (apply #'funcall-interactively function args))
          (t (apply function args)))))
     '((name . adh-jump))))
  (advice-add 'adh--minibuffer-pivot-call :filter-args #'adh--jump-pivot-args)
  (add-hook 'isearch-mode-hook #'adh--jump-isearch-start)
  (add-hook 'isearch-mode-end-hook #'adh--jump-isearch-end)
  (add-hook 'kill-buffer-hook #'adh--jump-kill-buffer)
  (add-hook 'window-configuration-change-hook #'adh--jump-prune-windows))

(adh--jump-install)

(provide 'adh-jump)

;;; adh-jump.el ends here
