;;; -*- lexical-binding: t; coding: utf-8 -*-

(defun adh--consult-fd-with-region (&optional i)
  "Run `consult-fd' under directory I, seeded with the active region if any."
  (if (use-region-p)
      (consult-fd i (buffer-substring-no-properties (region-beginning) (region-end)))
    (consult-fd i)))

(defun adh--consult-fd-directories (&optional arg)
  "Run `consult-fd' under ARG, restricted to directories."
  (require 'consult)
  (let ((consult-fd-args (concat adh--consult-fd-args " -t directory --prune")))
    (consult-fd arg)))

(defun adh--consult-ripgrep-with-region (&optional i)
  "Run `consult-ripgrep' under directory I, seeded with the active region if any."
  (if (use-region-p)
      (consult-ripgrep i (buffer-substring-no-properties (region-beginning) (region-end)))
    (consult-ripgrep i)))

(defun adh-consult-fd-here ()
  "Find files below `default-directory'."
  (interactive)
  (adh--consult-fd-with-region default-directory))

(defun adh-consult-fd-directories-here ()
  "Find directories below `default-directory'."
  (interactive)
  (adh--consult-fd-directories default-directory))

(defun adh-consult-ripgrep-here ()
  "Grep below `default-directory'."
  (interactive)
  (adh--consult-ripgrep-with-region default-directory))

(defun adh-consult-fd-project ()
  "Find files in the current project."
  (interactive)
  (adh--consult-fd-with-region (adh--get-project-dir)))

(defun adh-consult-fd-directories-project ()
  "Find directories in the current project."
  (interactive)
  (adh--consult-fd-directories (adh--get-project-dir)))

(defun adh-consult-ripgrep-project ()
  "Grep the current project."
  (interactive)
  (adh--consult-ripgrep-with-region (adh--get-project-dir)))

(defun adh-consult-fd-all ()
  "Find files from the filesystem root."
  (interactive)
  (adh--consult-fd-with-region 1))

(defun adh-consult-ripgrep-all ()
  "Grep from the filesystem root."
  (interactive)
  (adh--consult-ripgrep-with-region 1))

(defun adh-consult-line-with-region ()
  "Search lines in the buffer, seeded with the active region, with live preview."
  (interactive)
  (require 'consult)
  (let ((consult-preview-key 'any))
    (if (use-region-p)
        (let ((input (buffer-substring-no-properties (region-beginning) (region-end))))
          (deactivate-mark)
          (consult-line input))
      (consult-line))))

(defun adh-consult-imenu ()
  "Jump to an imenu entry with live preview."
  (interactive)
  (require 'consult)
  (let ((consult-preview-key 'any))
    (consult-imenu)))

(defun adh-consult-goto-line ()
  "Go to a line number with live preview."
  (interactive)
  (require 'consult)
  (let ((consult-preview-key 'any))
    (consult-goto-line)))

(defun adh-consult-locate (&optional initial)
  "Locate files by name, seeded with the active region or INITIAL."
  (interactive)
  (if (use-region-p)
      (consult-locate (buffer-substring-no-properties (region-beginning) (region-end)))
    (consult-locate initial)))

(defun adh-consult-select-window ()
  "Select another window, prompting by buffer name when several are eligible."
  (interactive)
  (require 'consult)
  (let* ((all-wins (cdr (window-list)))
         (wins (seq-remove (lambda (w)
                             (window-parameter w 'no-other-window))
                           all-wins)))
    (cond
     ((= (length wins) 0)
      (call-interactively #'other-window))
     ((= (length wins) 1)
      (select-window (car wins)))
     (t
      (let* ((cands (mapcar (lambda (w)
                              (cons (buffer-name (window-buffer w)) w))
                            wins))
             (choice (consult--read (mapcar #'car cands)
                                    :prompt "Window: "
                                    :require-match t
                                    :sort nil)))
        (when choice
          (select-window (cdr (assoc choice cands)))))))))

(use-package consult
  :ensure t
  :custom
  (consult-buffer-filter "\\*")
  (consult-narrow-key "C-,")
  (consult-preview-key "C-SPC")
  (consult-line-start-from-top t)
  :config
  (defconst adh--fd-executable-path (locate-user-emacs-file (concat "opt/fd/bin/fd" (when (eq system-type 'windows-nt) ".exe"))))
  (defconst adh--consult-fd-args  (concat adh--fd-executable-path " --sort-by-depth --full-path --hidden --no-ignore --color=never --exclude .git --path-separator=/"))
  (defvar adh--consult-ripgrep-args-base consult-ripgrep-args)

  (setq consult-fd-args (concat adh--consult-fd-args " -t file"))
  (setq consult-ripgrep-args (concat adh--consult-ripgrep-args-base " -P --hidden --no-ignore -g !TAGS -g !*.{git,zip,tar,gz,tgz,bz2,tbz2,xz,txz,zst,tzst,7z,rar,lz4,lzma,Z,jar,war}"))

  (pcase system-type
    ('windows-nt
     (adh-add-to-path "C:/Program Files/Everything")
     (setq consult-locate-args "es.exe -s -full-path-and-name"))
    (_
     (setq consult-locate-args "locate -i -r")))

  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark
  :ensure t
  :custom
  (embark-indicators
   '(embark-minimal-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  (embark-prompter 'embark-keymap-prompter)
  :hook
  ;; Give exported buffers a short "*e: MODE: QUERY*" name and dock them at the bottom.
  (embark-after-export . (lambda ()
                           (let ((bn (buffer-name)))
                             (when (string-match "\\*Embark Export: .* - \\(.*\\)\\*" bn)
                               (let ((search-input (match-string 1 bn)))
                                 (rename-buffer (format "*e: %s: %s" (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)) search-input) t)
                                 (adh-to-side-window)))))))

(use-package embark-consult
  :ensure t :after (consult embark))

(provide 'adh-consult)
