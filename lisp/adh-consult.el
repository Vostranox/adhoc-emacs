;;; -*- lexical-binding: t; coding: utf-8 -*-

(with-eval-after-load 'consult-register
  (cl-defmethod consult-register--describe ((val marker))
    "Describe marker register VAL as aligned \"buffer:line │ content\"."
    (pcase-let ((`(,buf-width . ,line-width)
                 (cl-loop for (_ . m) in (consult-register--alist 'noerror) if (markerp m)
                          maximize (length (buffer-name (marker-buffer m))) into bw and
                          maximize (length (number-to-string
                                            (with-current-buffer (marker-buffer m)
                                              (line-number-at-pos m t))))
                          into lw
                          finally return (cons (min 28 bw) lw))))
      (with-current-buffer (marker-buffer val)
        (save-excursion
          (without-restriction
            (goto-char val)
            (let* ((line (line-number-at-pos))
                   (bn (buffer-name))
                   (name (if (> (length bn) buf-width)
                             (concat "…" (substring bn (- (1- buf-width)))) bn))
                   (str (propertize (string-trim-left
                                     (consult--buffer-substring (pos-bol) (pos-eol) 'fontify))
                                    'consult-location (cons val line)))
                   (loc (concat (propertize name 'face 'consult-file)
                                (propertize ":" 'face 'shadow)
                                (propertize (number-to-string line) 'face 'consult-line-number))))
              (list (concat (string-pad loc (+ buf-width 1 line-width))
                            (propertize " │ " 'face 'shadow) str)
                    'multi-category `(consult-location . ,str)
                    'consult--type ?p)))))))

  (defun adh--consult-register-format (reg &optional completion)
    "Like `consult-register-format' but render the key as a bracketed [KEY]."
    (pcase-let* ((`(,key . ,val) reg)
                 (key-str (concat (propertize "[" 'face 'shadow)
                                  (propertize (single-key-description key)
                                              'face 'consult-highlight-match)
                                  (propertize "]" 'face 'shadow)))
                 (key-len (max 3 (length key-str)))
                 (`(,str . ,props) (consult-register--describe val)))
      (when (string-search "\n" str)
        (let* ((lines (seq-take (seq-remove #'string-blank-p (split-string str "\n")) 3))
               (space (cl-loop for x in lines minimize (string-match-p "[^ ]" x))))
          (setq str (mapconcat (lambda (x) (substring x space))
                               lines (concat "\n" (make-string (1+ key-len) ?\s))))))
      (setq str (concat
                 (and completion consult-register-prefix)
                 key-str (make-string (- key-len (length key-str)) ?\s) " "
                 str (and (not completion) "\n")))
      (when completion
        (add-text-properties 0 (length str) `(consult--candidate ,(car reg) ,@props) str))
      str))
  (advice-add #'consult-register-format :override #'adh--consult-register-format))

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

(defun adh-consult-point-to-register ()
  "Calls point-to-register."
  (interactive)
  (require 'consult)
  (call-interactively #'point-to-register))

(defun adh-consult-jump-to-register ()
  "Calls jump-to-register"
  (interactive)
  (require 'consult)
  (call-interactively #'jump-to-register)
  (recenter))

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

  (setq register-preview-delay 0.4
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

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
