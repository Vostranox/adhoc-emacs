;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-functions)

(defvar adh--consult-fd-args)

(defvar consult-fd-args)
(defvar consult-preview-key)
(defvar crm-prompt)

(defun adh--consult-fd-with-region (&optional i)
  "Run `consult-fd' under directory I, seeded with the region."
  (if (use-region-p)
      (consult-fd i (buffer-substring-no-properties (region-beginning) (region-end)))
    (consult-fd i)))

(defun adh--consult-fd-directories (&optional arg)
  "Run `consult-fd' under ARG, restricted to directories."
  (require 'consult)
  (let ((consult-fd-args (concat adh--consult-fd-args " -t directory --prune")))
    (consult-fd arg)))

(defun adh--consult-ripgrep-with-region (&optional i)
  "Run `consult-ripgrep' under directory I, seeded with the region."
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

(defun adh--read-dirs (&optional files)
  "Read comma-separated directories, or also files when FILES."
  (let ((crm-prompt "%p")
        (def (abbreviate-file-name default-directory))
        (minibuffer-completing-file-name t))
    (completing-read-multiple (if files "Dirs or files: " "Dirs: ")
                              #'completion-file-name-table
                              (unless files #'directory-name-p)
                              t def 'consult--path-history def)))

(defun adh-consult-fd-dirs (&optional initial)
  "Find files in comma-separated directories; INITIAL seeds the input."
  (interactive)
  (consult-fd (adh--read-dirs) initial))

(defun adh-consult-ripgrep-dirs (&optional initial)
  "Grep in comma-separated directories; INITIAL seeds the input."
  (interactive)
  (consult-ripgrep (adh--read-dirs t) initial))

(defun adh-consult-dirs-pivot ()
  "Rerun the current fd or ripgrep search in other directories."
  (interactive)
  (let ((input (minibuffer-contents-no-properties))
        (command (pcase (minibuffer-prompt)
                   ((rx bos "Fd") #'adh-consult-fd-dirs)
                   ((rx bos "Ripgrep") #'adh-consult-ripgrep-dirs)
                   (_ (user-error "Not in a consult fd or ripgrep search")))))
    (run-with-idle-timer adh--minibuffer-pivot-delay nil command input)
    (abort-recursive-edit)))

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

(defun adh-consult-locate (&optional initial)
  "Locate files by name, seeded with the active region or INITIAL."
  (interactive)
  (if (use-region-p)
      (consult-locate (buffer-substring-no-properties (region-beginning) (region-end)))
    (consult-locate initial)))

(defun adh-consult-flymake-show-buffer-diagnostics ()
  "Quit `consult-flymake' and list the buffer's Flymake diagnostics."
  (interactive)
  (let ((buf (window-buffer (minibuffer-selected-window))))
    (run-at-time 0 nil (lambda () (with-current-buffer buf (flymake-show-buffer-diagnostics))))
    (minibuffer-quit-recursive-edit)))

(defvar-keymap adh-consult-flymake-map)

(defun adh--imenu-marker (pos)
  (pcase pos
    ((pred markerp) pos)
    ((pred integerp) (copy-marker pos))
    (`(,p . ,_) (adh--imenu-marker p))))

(defun adh-embark-export-imenu (names)
  "Export imenu NAMES to an occur buffer linked to their definitions."
  (let ((items (if (minibufferp)
                   (with-minibuffer-selected-window (consult-imenu--items))
                 (consult-imenu--items))))
    (embark-consult-export-location-occur
     (delq nil
           (mapcar (lambda (name)
                     (when-let* ((marker (adh--imenu-marker (cdr (assoc name items)))))
                       (propertize name 'consult-location
                                   (cons marker
                                         (with-current-buffer (marker-buffer marker)
                                           (line-number-at-pos marker t))))))
                   names)))))

(use-package consult
  :ensure t :defer t
  :init
  (setq register-preview-delay 0.4
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  :custom
  (consult-buffer-filter '("\\` " "\\`\\*"))
  (consult-narrow-key "C-,")
  (consult-preview-key "C-SPC")
  (consult-line-start-from-top t)
  :config
  (plist-put consult-source-buffer :items
             (lambda () (consult--buffer-query
                         :sort 'visibility
                         :predicate #'adh--buffer-listable-p
                         :as #'consult--buffer-pair)))
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

  (consult-customize consult-flymake :keymap adh-consult-flymake-map)
  (consult-customize consult-imenu consult-goto-line :preview-key 'any))

(use-package embark
  :ensure t
  :custom
  (embark-indicators
   '(embark-minimal-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  (embark-prompter 'embark-keymap-prompter)
  :hook
  ;; Name exports "*e: MODE: QUERY*" and dock them at the bottom.
  (embark-after-export . (lambda ()
                           (let ((bn (buffer-name)))
                             (when (string-match "\\*Embark Export: .* - \\(.*\\)\\*" bn)
                               (let ((search-input (match-string 1 bn)))
                                 (rename-buffer (format "*e: %s: %s" (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)) search-input) t)))))))

(use-package embark-consult
  :ensure t :after (consult embark)
  :config
  (setf (alist-get 'imenu embark-exporters-alist) #'adh-embark-export-imenu))

(provide 'adh-consult)
