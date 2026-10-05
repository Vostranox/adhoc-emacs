;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-functions)

(defvar crm-prompt)
(defvar consult--regexp-compiler)
(defvar adh--imenu-items nil
  "Items that the last consult-imenu prompt offered.")

(defvar-keymap adh-consult-flycheck-map)

(defun adh--imenu-marker (pos)
  (pcase pos
    ((pred markerp) pos)
    ((pred integerp) (copy-marker pos))
    (`(,p . ,_) (adh--imenu-marker p))))

(defun adh--read-dirs (&optional start)
  "Read comma-separated directories, prefilled with START."
  (let ((crm-prompt "%p")
        (def (abbreviate-file-name (or start default-directory)))
        (minibuffer-completing-file-name t))
    (completing-read-multiple "Run in: " #'completion-file-name-table
                              #'directory-name-p t def 'consult--path-history def)))

(defun adh--consult-with-region (command dir)
  "Run consult COMMAND under DIR, seeded with the region."
  (let ((adh--command-origin-dir default-directory))
    (funcall command dir (and (use-region-p)
                              (replace-regexp-in-string
                               (rx (group (or bos " ")) "-") "\\1\\\\-"
                               (string-replace
                                " " "\\ "
                                (adh--pcre-quote
                                 (buffer-substring-no-properties (region-beginning) (region-end)))))))))

(defun adh-consult-fd-dirs (&optional initial start)
  "Find files in comma-separated directories; INITIAL seeds the input.
START prefills the directory prompt."
  (interactive)
  (let ((adh--command-origin-dir (or start default-directory)))
    (consult-fd (adh--read-dirs start) initial)))

(defun adh-consult-ripgrep-dirs (&optional initial start)
  "Grep in comma-separated directories; INITIAL seeds the input.
START prefills the directory prompt."
  (interactive)
  (let ((adh--command-origin-dir (or start default-directory)))
    (consult-ripgrep (adh--read-dirs start) initial)))

(defun adh-consult-fd-here ()
  "Find files below `default-directory'."
  (interactive)
  (adh--consult-with-region #'consult-fd default-directory))

(defun adh-consult-ripgrep-here ()
  "Grep below `default-directory'."
  (interactive)
  (adh--consult-with-region #'consult-ripgrep default-directory))

(defun adh-consult-fd-project ()
  "Find files in the current project."
  (interactive)
  (adh--consult-with-region #'consult-fd (adh--get-project-dir)))

(defun adh-consult-ripgrep-project ()
  "Grep the current project."
  (interactive)
  (adh--consult-with-region #'consult-ripgrep (adh--get-project-dir)))

(defun adh-consult-dirs-pivot ()
  "Rerun the current fd or ripgrep search in other directories."
  (interactive)
  (let ((input (minibuffer-contents-no-properties))
        (start (adh--origin-dir))
        (command (pcase (minibuffer-prompt)
                   ((rx bos "Fd") #'adh-consult-fd-dirs)
                   ((rx bos "Ripgrep") #'adh-consult-ripgrep-dirs)
                   (_ (user-error "Not in a consult fd or ripgrep search")))))
    (adh--minibuffer-pivot-call
     (lambda ()
       (let ((default-directory start))
         (funcall command input start))))))

(defun adh-consult-root-pivot ()
  "Rerun the current fd or ripgrep search at the project root.
When it already runs there, rerun it in the directory it started from."
  (interactive)
  (let* ((input (minibuffer-contents-no-properties))
         (origin (adh--origin-dir))
         (root (adh--get-project-dir origin))
         (dir (if (and root (not (file-equal-p default-directory root))) root origin))
         (command (pcase (minibuffer-prompt)
                    ((rx bos "Fd") #'consult-fd)
                    ((rx bos "Ripgrep") #'consult-ripgrep)
                    (_ (user-error "Not in a consult fd or ripgrep search")))))
    (when (file-equal-p dir default-directory)
      (user-error (if root "Already at the project root" "Not in a project")))
    (adh--minibuffer-pivot-call
     (lambda ()
       (let ((adh--command-origin-dir origin))
         (funcall command dir input))))))

(defun adh-consult-locate (&optional initial)
  "Locate files by name, seeded with the active region or INITIAL."
  (interactive)
  (consult-locate (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    initial)))

(defun adh-consult-flycheck-show-buffer-diagnostics ()
  "Quit `consult-flycheck' and list the buffer's Flycheck diagnostics."
  (interactive)
  (let ((buf (window-buffer (minibuffer-selected-window))))
    (run-at-time 0 nil (lambda () (with-current-buffer buf (flycheck-list-errors))))
    (minibuffer-quit-recursive-edit)))

(defun adh--imenu-remember-items (_prompt items)
  "Advice: keep the ITEMS a consult-imenu prompt offers, for the export."
  (setq adh--imenu-items items))

(defun adh-embark-export-imenu (names)
  "Export the definition lines of imenu NAMES to an occur buffer."
  (let ((items (cond ((memq (bound-and-true-p embark--command) '(consult-imenu consult-imenu-multi))
                      adh--imenu-items)
                     ((minibufferp) (with-minibuffer-selected-window (consult-imenu--items)))
                     (t (consult-imenu--items))))
        seen)
    (embark-consult-export-location-occur
     (delq nil
           (mapcar (lambda (name)
                     (when-let* ((marker (adh--imenu-marker (cdr (assoc name items)))))
                       (with-current-buffer (marker-buffer marker)
                         (save-restriction
                           (widen)
                           (save-excursion
                             (goto-char marker)
                             (let ((line (cons (current-buffer) (line-number-at-pos))))
                               (unless (member line seen)
                                 (push line seen)
                                 (propertize (buffer-substring (pos-bol) (pos-eol))
                                             'consult-location (cons marker (cdr line))))))))))
                   names)))))

(defvar adh--grep-lookahead-cache (make-hash-table :test #'equal)
  "Results of `consult--grep-lookahead-p', per host and command.")

(defun adh--consult-cache-lookahead (fn &rest cmd)
  "Advice: run the look-ahead check FN on CMD once per host and command."
  (car (with-memoization (gethash (cons (file-remote-p default-directory) cmd)
                                  adh--grep-lookahead-cache)
         (list (apply fn cmd)))))

(defun adh--consult-scope (word origin files)
  "Return (PATH BEG END) for each path the ./ or ../ WORD names from ORIGIN."
  (let* ((full (expand-file-name word origin))
         (parent (file-name-directory full))
         (part (file-name-nondirectory (directory-file-name word)))
         (len (if (member part '("." "..")) 0 (length part))))
    (mapcar (lambda (path)
              (let* ((path (file-relative-name path))
                     (beg (length (file-name-directory (directory-file-name path)))))
                (list path beg (+ beg len))))
            (cond ((file-directory-p full) (list (file-name-as-directory full)))
                  ((and files (file-exists-p full)) (list full))
                  ((and (file-directory-p parent)
                        (mapcar (lambda (f) (expand-file-name f parent))
                                (let ((case-fold-search nil))
                                  (completion-pcm--filename-try-filter
                                   (seq-filter (if files #'always #'directory-name-p)
                                               (file-name-all-completions
                                                (file-name-nondirectory full) parent)))))))
                  (t (list full))))))

(defun adh--pcre-to-emacs-regexp (re)
  "Approximate PCRE RE in Emacs syntax for highlighting, dropping lookarounds."
  (replace-regexp-in-string
   (rx (or (seq "\\" anything)
           (seq "[" (? "^") (? "]") (* (or (seq "\\" anything) (not (any "]\\")))) "]")
           "(?:" (any "(){}|")))
   (lambda (m)
     (pcase m
       ((pred (string-prefix-p "["))
        (string-replace "\\d" "[:digit:]" (string-replace "\\s" "[:space:]" (string-replace "\\w" "[:word:]" m))))
       ((or "(" "(?:") "\\(?:")
       ((or ")" "{" "}" "|") (concat "\\" m))
       ((or "\\(" "\\)" "\\{" "\\}" "\\|") (substring m 1))
       ("\\d" "[0-9]") ("\\D" "[^0-9]") ("\\s" "[[:space:]]") ("\\S" "[^[:space:]]")
       (_ m)))
   (replace-regexp-in-string
    (rx "(?" (or (seq (? "<") (any "=!") (* (or (seq "\\" anything) (not (any "()\\")))) ")")
                 (seq (+ (any "a-z")) ")")))
    ""
    (replace-regexp-in-string (rx "(?" (? "P") "<" (+ (any word "_")) ">") "(" re))
   t t))

(defun adh--consult-pcre-compiler (input _type ignore-case)
  "Pass the words of INPUT to the tool as PCRE, highlighting an approximation."
  (let ((regexps (consult--split-escaped input)))
    (cons (if (cdr regexps) (mapcar (lambda (r) (concat "(?:" r ")")) regexps) regexps)
          (when-let* ((hl (seq-filter #'consult--valid-regexp-p
                                      (mapcar #'adh--pcre-to-emacs-regexp regexps))))
            (apply-partially #'consult--highlight-regexps hl ignore-case)))))

(defun adh--consult-scoped-builder (make-builder paths files)
  "Builder from MAKE-BUILDER where input words like ./ or ../x set the PATHS."
  (let ((origin (adh--origin-dir))
        (builder (funcall make-builder paths))
        cache)
    (lambda (input)
      (let* ((consult--regexp-compiler #'adh--consult-pcre-compiler)
             (words (split-string (string-replace "\\ " "\0" input) " "))
             (rel (seq-filter (lambda (w) (string-match-p "\\`\\.\\.?/" w)) words))
             (rest (string-replace "\0" "\\ " (string-join (seq-difference words rel) " "))))
        (if (not rel)
            (funcall builder input)
          (condition-case err
              (let* ((spans (mapcan (lambda (w) (adh--consult-scope (string-replace "\0" " " w) origin files))
                                    rel))
                     (dirs (mapcar #'car spans)))
                (unless (equal (car cache) dirs)
                  (setq cache (cons dirs (funcall make-builder dirs))))
                (let ((res (funcall (cdr cache) rest)))
                  (if (or files (not (consp res)))
                      res
                    (let ((hl (cdr res)))
                      (cons (car res)
                            (lambda (str)
                              (when hl (funcall hl str))
                              (pcase-dolist (`(,d ,beg ,end) spans)
                                (when (string-prefix-p d str)
                                  (add-face-text-property beg end 'consult-highlight-match nil str)))
                              str))))))
            (error
             (message "[adh] ./ search scope failed: %s" (error-message-string err))
             (funcall builder rest))))))))

(defun adh--consult-fd-scoped (make-builder paths)
  "Advice: `adh--consult-scoped-builder' for fd, defaulting to -t file."
  (let ((builder (adh--consult-scoped-builder make-builder paths nil)))
    (lambda (input)
      (let ((res (funcall builder input)))
        (when (and (consp (car-safe res))
                   (not (seq-some (lambda (opt) (string-match-p "\\`\\(-t\\|--type\\)" opt))
                                  (cdr (consult--command-split input)))))
          (setcar res (cons (caar res) (append '("-t" "file") (cdar res)))))
        res))))

(defun adh--consult-ripgrep-scoped (make-builder paths)
  "Advice: `adh--consult-scoped-builder' for ripgrep, which also takes files."
  (adh--consult-scoped-builder make-builder paths t))

(use-package consult
  :ensure t :defer t
  :init
  (setq register-preview-delay 0.4
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  :custom
  (consult-buffer-filter '("\\` " "\\`\\*"))
  (consult-narrow-key "C-,")
  (consult-preview-key "M-SPC")
  (consult-line-start-from-top t)
  :config
  (plist-put consult-source-buffer :items
             (lambda () (consult--buffer-query
                         :sort 'visibility
                         :predicate #'adh--buffer-listable-p
                         :as #'consult--buffer-pair)))

  (defvar adh--consult-ripgrep-args-base consult-ripgrep-args)

  (setq consult-fd-args '(adh--fd-program "--sort-by-depth --hidden --no-ignore --prune --color=never --exclude .git --path-separator=/"))
  (setq consult-ripgrep-args (concat adh--consult-ripgrep-args-base " --hidden --no-ignore -g !TAGS -g !*.{git,zip,tar,gz,tgz,bz2,tbz2,xz,txz,zst,tzst,7z,rar,lz4,lzma,Z,jar,war}"))

  (advice-add 'consult--fd-make-builder :around #'adh--consult-fd-scoped)
  (advice-add 'consult--ripgrep-make-builder :around #'adh--consult-ripgrep-scoped)
  (advice-add 'consult--grep-lookahead-p :around #'adh--consult-cache-lookahead)
  (advice-add 'consult-imenu--select :before #'adh--imenu-remember-items)

  (pcase system-type
    ('windows-nt
     (adh-add-to-path "C:/Program Files/Everything")
     (setq consult-locate-args "es.exe -s -full-path-and-name"))
    ('darwin
     (setq consult-locate-args "locate -i"))
    (_
     (setq consult-locate-args "locate -i -r")))

  (consult-customize consult-imenu consult-goto-line :preview-key 'any)
  (consult-customize consult-imenu-multi consult-goto-line :preview-key 'any))

(use-package consult-flycheck
  :ensure t :defer t
  :config
  (consult-customize consult-flycheck :keymap adh-consult-flycheck-map))

(use-package embark
  :ensure t :defer t
  :custom
  (embark-indicators
   '(embark-minimal-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  :hook
  (embark-after-export . (lambda ()
                           (let ((bn (buffer-name)))
                             (when (string-match "\\*Embark Export: .*? - \\(.*\\)\\*" bn)
                               (let ((search-input (match-string 1 bn)))
                                 (rename-buffer (format "*e: %s: %s" (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)) search-input) t)))))))

(use-package embark-consult
  :ensure t :after (consult embark)
  :config
  (setf (alist-get 'imenu embark-exporters-alist) #'adh-embark-export-imenu))

(provide 'adh-consult)
