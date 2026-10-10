;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-functions)

(eval-when-compile
  (when (bound-and-true-p byte-compile-current-file)
    (require 'magit)))

(declare-function adh-picker-define-preview "adh-picker")
(declare-function adh-picker-preview-buffer "adh-picker")

(defvar adh--magit-submodule-foreach-history nil
  "Minibuffer history of `adh-magit-submodule-foreach'.")
(defvar adh--git-show-cache nil
  "Recent commit previews, most recent first, as ((DIR HASH FILES) . TEXT).")
(defconst adh--git-commit-format "%h%x00%ad%x00%an%x00%D%x00%s"
  "Git log format of the fields `adh--git-commit-candidate' takes.")
(defvar adh--magit-show-full-commit nil
  "Non-nil while `adh-magit-show-commit-original' runs.")

(define-derived-mode magit-staging-mode magit-status-mode "magit-staging"
  "Like `magit-status-mode' but limited to staged/unstaged changes."
  :group 'magit-status)

(defun magit-staging-refresh-buffer ()
  "Populate a `magit-staging-mode' buffer with just the change sections."
  (magit-insert-section (status)
    (magit-insert-status-headers)
    (magit-insert-unstaged-changes)
    (magit-insert-staged-changes)))

(defun adh--magit-show-commit-current-file (fn rev &optional args files module)
  "Call FN for REV with ARGS, FILES and MODULE, limiting the diff during blame.
In a blame buffer, replace FILES with the blamed file, or nil when
`adh--magit-show-full-commit' requests the whole commit."
  (when-let* ((chunk (and (bound-and-true-p magit-blame-mode) (magit-current-blame-chunk))))
    (setq files (unless adh--magit-show-full-commit (list (oref chunk orig-file)))))
  (funcall fn rev args files module))

(defun adh--git-toplevel ()
  "Return the top directory of the current repository."
  (require 'magit)
  (or (magit-toplevel) (user-error "Not in a Git repository")))

(defun adh--git-output (&rest args)
  "Return the output of git with ARGS, run in `default-directory'.
Signal git's message if it fails; exit status 1 counts as success, as
`git diff' uses it for differences."
  (let ((errors (make-temp-file "adh-git")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'process-file "git" nil (list t errors) nil "--no-pager" args)))
            (unless (memq status '(0 1))
              (user-error "git %s: %s" (car args)
                          (string-trim (with-temp-buffer
                                         (insert-file-contents errors)
                                         (buffer-string))))))
          (buffer-string))
      (delete-file errors))))

(defun adh--git-read (prompt category cands)
  "Read one of CANDS with PROMPT, as candidates of completion CATEGORY.
Keep the order of CANDS and return the chosen one with its text properties."
  (unless cands
    (user-error "Nothing to pick"))
  (let ((choice (completing-read
                 prompt
                 (lambda (string pred action)
                   (if (eq action 'metadata)
                       `(metadata (category . ,category)
                                  (display-sort-function . identity)
                                  (cycle-sort-function . identity))
                     (complete-with-action action cands string pred)))
                 nil t)))
    (or (car (member choice cands))
        (user-error "No such candidate: %s" choice))))

(defun adh--git-commit-candidate (fields &rest props)
  "Return a commit candidate from FIELDS (HASH DATE AUTHOR REFS SUBJECT).
The candidate carries its hash in `adh-git-commit', and PROPS."
  (pcase-let ((`(,hash ,date ,author ,refs ,subject) fields))
    (apply #'propertize
           (concat (propertize hash 'face 'magit-hash) "  "
                   (propertize date 'face 'magit-log-date) "  "
                   (unless (string-empty-p refs)
                     (concat (propertize (format "(%s)" refs) 'face 'magit-branch-local) " "))
                   subject "  "
                   (propertize author 'face 'magit-log-author))
           'adh-git-commit hash props)))

(defun adh--git-log-candidates (file)
  "Return the commits of the repository, or only those that changed FILE.
A commit that changed FILE carries its name at that commit, which differs
before a rename, in `adh-git-files'."
  (let ((entries
         (split-string
          (apply #'adh--git-output "--literal-pathspecs" "log" "--no-color"
                 "--date=short" "--max-count=2000" "-z"
                 (concat "--format=%x00" adh--git-commit-format)
                 (and file (list "--name-only" "--follow" "--" file)))
          "\0"))
        cands)
    (while entries
      (while (and entries (string-empty-p (car entries))) (pop entries))
      (when entries
        (let ((fields (cl-loop repeat 5 collect (pop entries)))
              path)
          (when file
            (when (and entries (not (string-empty-p (car entries))))
              (setq path (string-remove-prefix "\n" (pop entries)))))
          (push (adh--git-commit-candidate
                 fields 'adh-git-files (and file (list (or path file))))
                cands))))
    (nreverse cands)))

(defun adh--git-patch-file (patch)
  "Return the destination filename in PATCH, decoding Git's quoting.
Ignore the tab separating an unquoted filename from a patch timestamp."
  (when (string-match "^\\+\\+\\+ \\([^\n\t]+\\)" patch)
    (let ((path (magit-decode-git-path (match-string 1 patch))))
      (when (string-prefix-p "b/" path)
        (substring path 2)))))

(defun adh--git-diff-base ()
  "Return HEAD, or the empty tree's hash before the first commit.
Compute the hash using this repository's object format, without writing
an object or changing the index."
  (or (magit-rev-verify "HEAD")
      (string-trim (adh--git-output "hash-object" "-t" "tree" "--stdin"))))

(defun adh--git-show-preview (cand)
  "Return a buffer with the diff of the commit candidate CAND."
  (when-let* ((hash (get-text-property 0 'adh-git-commit cand)))
    (let* ((files (get-text-property 0 'adh-git-files cand))
           (key (list default-directory hash files))
           (hit (assoc key adh--git-show-cache))
           (text (or (cdr hit)
                     (apply #'adh--git-output "--literal-pathspecs" "show"
                            "--no-color" "--stat" "--patch" hash
                            (and files (cons "--" files))))))
      (setq adh--git-show-cache (cons (cons key text) (delq hit adh--git-show-cache)))
      (when (nthcdr 20 adh--git-show-cache)
        (setcdr (nthcdr 19 adh--git-show-cache) nil))
      (adh-picker-preview-buffer "adh-git-show" text #'diff-mode))))

(defun adh--git-line-log-candidates (file beg end)
  "Return the commits that changed lines BEG to END of FILE.
Each candidate carries its patch of those lines in `adh-git-patch'."
  (let ((log (adh--git-output "log" "--no-color" "--date=short"
                              "--src-prefix=a/" "--dst-prefix=b/"
                              (format "-L%d,%d:%s" beg end file)
                              (concat "--format=%x01" adh--git-commit-format))))
    (mapcar (lambda (entry)
              (let ((eol (or (string-search "\n" entry) (length entry))))
                (let ((patch (string-trim (substring entry eol))))
                  (adh--git-commit-candidate
                   (split-string (substring entry 0 eol) "\0")
                   'adh-git-patch patch
                   ;; The file's name at that commit, which differs before a rename.
                   'adh-git-files (list (or (adh--git-patch-file patch) file))))))
            (split-string log "\1" t))))

(defun adh--git-patch-preview (cand)
  "Return a buffer with the patch the line history candidate CAND carries."
  (when-let* ((patch (get-text-property 0 'adh-git-patch cand)))
    (adh-picker-preview-buffer "adh-git-patch" patch #'diff-mode)))

(defun adh--git-status-candidates ()
  "Return the changed and untracked files of the current repository.
Each candidate carries its path in `adh-git-file' and its status in
`adh-git-status'."
  (let ((entries (split-string (adh--git-output "status" "--porcelain=v1" "-z"
                                                "--untracked-files=all")
                               "\0" t))
        cands)
    (while entries
      (let* ((entry (pop entries))
             (status (substring entry 0 2))
             (file (substring entry 3)))
        (when (string-match-p "[RC]" status)
          (pop entries))
        (push (propertize
               (concat (propertize status 'face
                                   (pcase status
                                     ("??" 'shadow)
                                     ((rx "D") 'error)
                                     ((rx bos (not " ")) 'success)
                                     (_ 'warning)))
                       " " file)
               'adh-git-file file 'adh-git-status status)
              cands)))
    (nreverse cands)))

(defun adh--git-status-preview (cand)
  "Return a buffer with the changes of the changed file candidate CAND."
  (when-let* ((file (get-text-property 0 'adh-git-file cand)))
    (adh-picker-preview-buffer
     "adh-git-diff"
     (if (equal (get-text-property 0 'adh-git-status cand) "??")
         (adh--git-output "diff" "--no-color" "--no-ext-diff" "--no-index" "--" "/dev/null" file)
       (adh--git-output "--literal-pathspecs" "diff" "--no-color" "--no-ext-diff"
                        (adh--git-diff-base) "--" file))
     #'diff-mode)))

(defun adh--git-hunk-candidates ()
  "Return the hunks of the changes since the last commit."
  (with-temp-buffer
    (insert (adh--git-output "diff" "--no-color"
                            "--no-ext-diff" "--src-prefix=a/" "--dst-prefix=b/"
                            (adh--git-diff-base)))
    (goto-char (point-min))
    (let (cands header file)
      (while (re-search-forward "^\\(diff --git \\|@@ -[0-9,]+ \\+\\([0-9]+\\)\\)" nil t)
        (let ((start (match-beginning 0)))
          (if (not (match-beginning 2))
              (let ((hunks (save-excursion
                             (if (re-search-forward "^@@ " nil t) (match-beginning 0) (point-max)))))
                (setq header (buffer-substring-no-properties start hunks)
                      file (adh--git-patch-file header)))
            (let* ((line (string-to-number (match-string 2)))
                   (end (save-excursion
                          (forward-line)
                          (if (re-search-forward "^\\(diff --git \\|@@ \\)" nil t)
                              (match-beginning 0)
                            (point-max))))
                   (body (buffer-substring-no-properties start end))
                   (change (save-excursion
                             (forward-line)
                             (while (and (< (point) end) (eq (char-after) ?\s))
                               (setq line (1+ line))
                               (forward-line))
                             (buffer-substring-no-properties (point) (pos-eol)))))
              (when file
                (push (propertize
                       (concat (propertize (format "%s:%d" file line) 'face 'magit-filename)
                               "  "
                               (propertize change 'face
                                           (if (string-prefix-p "-" change)
                                               'diff-removed 'diff-added)))
                       'adh-git-hunk (concat header body)
                       'adh-git-file file 'adh-git-line line)
                      cands))))))
      (nreverse cands))))

(defun adh--git-hunk-preview (cand)
  "Return a buffer with the hunk the hunk candidate CAND carries."
  (when-let* ((hunk (get-text-property 0 'adh-git-hunk cand)))
    (adh-picker-preview-buffer "adh-git-hunk" hunk #'diff-mode)))

(defun adh--git-branch-candidates ()
  "Return the local and remote branches, most recently committed to first."
  (delq nil
        (mapcar
         (lambda (line)
           (pcase-let* ((`(,head ,ref ,date ,subject) (split-string line "\0"))
                        (remote (string-prefix-p "refs/remotes/" ref))
                        (name (string-remove-prefix (if remote "refs/remotes/" "refs/heads/") ref)))
             (unless (string-suffix-p "/HEAD" name)
               (propertize
                (concat (if (equal head "*") "* " "  ")
                        (propertize name 'face (if remote 'magit-branch-remote 'magit-branch-local))
                        "  " (propertize date 'face 'magit-log-date)
                        "  " subject)
                'adh-git-branch name 'adh-git-ref ref 'adh-git-remote remote))))
         (process-lines "git" "for-each-ref" "--sort=-committerdate"
                        "--format=%(HEAD)%00%(refname)%00%(committerdate:relative)%00%(subject)"
                        "refs/heads" "refs/remotes"))))

(defun adh--git-branch-preview (cand)
  "Return a buffer with the latest commits of the branch candidate CAND."
  (when-let* ((branch (get-text-property 0 'adh-git-ref cand)))
    (adh-picker-preview-buffer
     "adh-git-branch"
     (mapconcat (lambda (line)
                  (pcase-let ((`(,hash ,date ,author ,refs ,subject) (split-string line "\0")))
                    (concat (propertize hash 'face 'magit-hash) "  "
                            (propertize date 'face 'magit-log-date) "  "
                            (unless (string-empty-p refs)
                              (concat (propertize (format "(%s)" refs) 'face 'magit-branch-local) " "))
                            subject "  " (propertize author 'face 'magit-log-author))))
                (process-lines "git" "log" "--no-color" "--date=short" "--max-count=100"
                               (concat "--format=" adh--git-commit-format) branch "--")
                "\n"))))

(defun adh--git-stash-candidates ()
  "Return the stashes, each carrying its name in `adh-git-stash'."
  (mapcar (lambda (line)
            (pcase-let ((`(,stash ,date ,subject) (split-string line "\0")))
              (propertize (concat (propertize stash 'face 'magit-hash) "  "
                                  (propertize date 'face 'magit-log-date) "  " subject)
                          'adh-git-stash stash)))
          (process-lines "git" "stash" "list" "--format=%gd%x00%cr%x00%gs")))

(defun adh--git-stash-preview (cand)
  "Return a buffer with the changes of the stash candidate CAND."
  (when-let* ((stash (get-text-property 0 'adh-git-stash cand)))
    (adh-picker-preview-buffer
     "adh-git-stash"
     (adh--git-output "stash" "show" "--no-color" "--stat" "--patch" stash)
     #'diff-mode)))

(defun adh-git-commit-toggle-diff ()
  "Toggle the inline diff without moving point in the commit message."
  (interactive)
  (unless (bound-and-true-p git-commit-mode)
    (user-error "Not a Git commit message buffer"))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (unless (and (re-search-forward
                    (format "^%s -+ >8 -+" (regexp-quote comment-start)) nil t)
                   (push-button (line-beginning-position)))
        (user-error "No inline diff in this commit message")))))

(defun adh-magit-staging ()
  "Open the trimmed staging-only magit buffer."
  (interactive)
  (require 'magit)
  (magit-setup-buffer #'magit-staging-mode))

(defun adh-magit-staging-quick ()
  "Show a hidden staging buffer or open one; a prefix arg forces a new one."
  (interactive)
  (require 'magit)
  (if-let* ((buffer
            (and (not current-prefix-arg)
                 (not (magit-get-mode-buffer 'magit-staging-mode nil 'selected))
                 (magit-get-mode-buffer 'magit-staging-mode))))
      (magit-display-buffer buffer)
    (adh-magit-staging)))

(defun adh-magit-show-commit-original ()
  "Show the full commit at point, bypassing the blame narrowing."
  (interactive)
  (let ((adh--magit-show-full-commit t))
    (call-interactively #'magit-show-commit)))

(defun adh-magit-restore-current ()
  "Discard unstaged changes to this file or directory (git restore)."
  (interactive)
  (require 'magit)
  (let ((path (expand-file-name (or (adh--buffer-file-name) default-directory))))
    (when (y-or-n-p (format "Restore %s? "
                            (file-relative-name path (magit-toplevel (file-name-directory path)))))
      (magit-call-git "restore" path)
      (when-let* ((buf (get-file-buffer path)))
        (with-current-buffer buf
          (revert-buffer :ignore-auto :noconfirm))))))

(defun adh-switch-magit-buffer ()
  "Switch to a Magit buffer."
  (interactive)
  (adh-switch-buffer-of-mode 'magit-status-mode "Magit: "))

(defun adh-magit-status-dwim ()
  "Show this repo's status without refreshing, else prompt for a repo."
  (interactive)
  (require 'magit)
  (call-interactively (if (magit-toplevel) #'magit-status-quick #'magit-status)))

(defun adh-magit-visit-file-dwim (&optional other-window)
  "Visit the worktree file from staged file headings, otherwise as usual.
With a prefix argument OTHER-WINDOW, display the buffer in another window."
  (interactive "P")
  (if (eq (magit-diff-type) 'staged)
      (magit-diff-visit-worktree-file other-window)
    (magit-diff-visit-file other-window)))

(defun adh-magit-visit-thing-other-window ()
  "Visit whatever RET would visit at point, in another window."
  (interactive)
  (let ((cmd (key-binding (kbd "RET"))))
    (cond ((memq cmd '(adh-magit-visit-file-dwim magit-diff-visit-file))
           (funcall cmd t))
          (t
           (let ((display-buffer-overriding-action '(nil (inhibit-same-window . t))))
             (call-interactively (or cmd #'magit-visit-thing)))))))

(defun adh-magit-preview-thing ()
  "Visit the thing at point in another window, keeping point here."
  (interactive)
  (save-selected-window
    (adh-magit-visit-thing-other-window)))

(defun adh-magit-blame-copy-short-hash ()
  "Copy the 7-character short hash of the blamed commit to the kill ring."
  (interactive)
  (kill-new (message "%s" (substring (oref (magit-current-blame-chunk) orig-rev) 0 7))))

(defun adh-toggle-magit-blame ()
  "Toggle `magit-blame' for the current file."
  (interactive)
  (if (bound-and-true-p magit-blame-mode)
      (magit-blame-mode 0)
    (call-interactively 'magit-blame-addition)))

(defun adh-magit-log-buffer-file-follow ()
  "Show the log for the current file, following it across renames."
  (interactive)
  (let ((magit-direct-use-buffer-arguments 'never))
    (magit-log-buffer-file t)))

(defun adh-magit-log-trace-region-or-line ()
  "Show the line-history (git log -L) of the region, or the current line."
  (interactive)
  (require 'magit)
  (let ((line (line-number-at-pos nil t))
        (magit-log-buffer-file-locked nil))
    (apply #'magit-log-buffer-file nil
           (or (magit-file-region-line-numbers) (list line line)))))

(defun adh-magit-submodule-update-all ()
  "Run git submodule update --init --recursive with the menu's arguments."
  (interactive)
  (require 'magit)
  (magit-with-toplevel
    (magit-run-git-async "submodule" "update" "--init" "--recursive"
                         (magit-submodule-arguments "--force" "--remote" "--no-fetch"
                                                    "--checkout" "--rebase" "--merge"))))

(defun adh-magit-submodule-foreach ()
  "Read the rest of a git submodule foreach command and run it at the top level."
  (interactive)
  (require 'magit)
  (magit-with-toplevel
    (let ((prefix (concat "git submodule foreach "
                          (and (magit-submodule-arguments "--recursive") "--recursive "))))
      (magit-shell-command-topdir
       (concat prefix (read-shell-command prefix nil 'adh--magit-submodule-foreach-history))))))

(defun adh-git-log ()
  "Pick a commit of the current repository and show it.
In the picker, the preview shows each commit's diff."
  (interactive)
  (let ((default-directory (adh--git-toplevel)))
    (magit-show-commit
     (get-text-property 0 'adh-git-commit
                        (adh--git-read "Commits: " 'adh-git-commit
                                       (adh--git-log-candidates nil))))))

(defun adh-git-log-file ()
  "Pick a commit that changed the current file and show its changes to the file.
In the picker, the preview shows each commit's diff of the file."
  (interactive)
  (let* ((file (or (progn (require 'magit) (magit-file-relative-name nil t))
                   (user-error "Buffer is not visiting a file tracked by Git")))
         (default-directory (adh--git-toplevel))
         (cand (adh--git-read (format "Commits of %s: " file) 'adh-git-commit
                              (adh--git-log-candidates file))))
    (magit-show-commit (get-text-property 0 'adh-git-commit cand) nil
                       (get-text-property 0 'adh-git-files cand))))

(defun adh-git-log-line ()
  "Pick a commit that changed the current line, or the region's lines, and show it.
In the picker, the preview shows each commit's change to those lines, as
of the last commit of the file."
  (interactive)
  (require 'magit)
  (let* ((file (or (magit-file-relative-name nil t)
                   (user-error "Buffer is not visiting a file tracked by Git")))
         (lines (if (use-region-p)
                    (list (line-number-at-pos (region-beginning) t)
                          (line-number-at-pos (max (region-beginning) (1- (region-end))) t))
                  (let ((line (line-number-at-pos nil t))) (list line line))))
         (default-directory (adh--git-toplevel))
         (cand (adh--git-read (format "Commits of %s:%s: " file (car lines))
                              'adh-git-line-commit
                              (adh--git-line-log-candidates file (car lines) (cadr lines)))))
    (magit-show-commit (get-text-property 0 'adh-git-commit cand) nil
                       (get-text-property 0 'adh-git-files cand))))

(defun adh-git-status ()
  "Pick a changed or untracked file of the current repository and visit it.
In the picker, the preview shows each file's changes since the last commit."
  (interactive)
  (let* ((default-directory (adh--git-toplevel))
         (file (get-text-property 0 'adh-git-file
                                  (adh--git-read "Changed files: " 'adh-git-status
                                                 (adh--git-status-candidates)))))
    (if (file-exists-p file)
        (find-file (expand-file-name file))
      (magit-diff-range "HEAD" nil (list file)))))

(defun adh-git-hunks ()
  "Pick a hunk of the changes since the last commit and go to it.
In the picker, the preview shows each hunk."
  (interactive)
  (let* ((default-directory (adh--git-toplevel))
         (hunk (adh--git-read "Hunks: " 'adh-git-hunk (adh--git-hunk-candidates))))
    (find-file (expand-file-name (get-text-property 0 'adh-git-file hunk)))
    (goto-char (point-min))
    (forward-line (1- (get-text-property 0 'adh-git-line hunk)))))

(defun adh-git-branches ()
  "Pick a branch and check it out; a remote one gets a local tracking branch.
Reuse a local branch only when it tracks the selected remote branch.
In the picker, the preview shows each branch's latest commits."
  (interactive)
  (let* ((default-directory (adh--git-toplevel))
         (cand (adh--git-read "Branches: " 'adh-git-branch (adh--git-branch-candidates)))
         (branch (get-text-property 0 'adh-git-branch cand)))
    (if (not (get-text-property 0 'adh-git-remote cand))
        (magit-run-git "checkout" branch "--")
      (let* ((ref (get-text-property 0 'adh-git-ref cand))
             (remote (car (sort (seq-filter
                                (lambda (name) (string-prefix-p (concat name "/") branch))
                                (magit-list-remotes))
                               (lambda (a b) (> (length a) (length b))))))
             (local (and remote (substring branch (1+ (length remote))))))
        (unless local
          (user-error "No configured remote for %s" branch))
        (if (magit-rev-verify (concat "refs/heads/" local))
            (if (equal (magit-get-upstream-ref local) ref)
                (magit-run-git "checkout" local "--")
              (user-error "Local branch %s does not track %s; choose another local branch name in Magit"
                          local branch))
          (magit-run-git "checkout" "--track" "-b" local ref "--"))))))

(defun adh-git-stash ()
  "Pick a stash and show it in Magit, where it can be applied or popped.
In the picker, the preview shows each stash's changes."
  (interactive)
  (let ((default-directory (adh--git-toplevel)))
    (magit-stash-show
     (get-text-property 0 'adh-git-stash
                        (adh--git-read "Stashes: " 'adh-git-stash (adh--git-stash-candidates))))))

(with-eval-after-load 'adh-picker
  (adh-picker-define-preview 'adh-git-commit #'adh--git-show-preview)
  (adh-picker-define-preview 'adh-git-line-commit #'adh--git-patch-preview t)
  (adh-picker-define-preview 'adh-git-status #'adh--git-status-preview)
  (adh-picker-define-preview 'adh-git-hunk #'adh--git-hunk-preview t)
  (adh-picker-define-preview 'adh-git-branch #'adh--git-branch-preview)
  (adh-picker-define-preview 'adh-git-stash #'adh--git-stash-preview))

(use-package magit
  :ensure t :defer 10
  :init
  (setq magit-auto-revert-mode nil)
  :custom
  (magit-refresh-verbose t)
  (magit-commit-show-diff nil)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-revision-filter-files-on-follow t)
  (magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18))
  (magit-section-initial-visibility-alist
   '((staged . hide) (unstaged . hide) (untracked . hide) (stashes . hide) (unpushed . hide) (unpulled . hide)))
  :config
  (put 'magit-log-mode 'magit-log-default-arguments '("-n1024"))
  (keymap-set magit-file-section-map "<remap> <magit-visit-thing>" #'adh-magit-visit-file-dwim)
  (advice-add #'magit-show-commit :around #'adh--magit-show-commit-current-file)
  :hook
  (magit-mode . (lambda () (let ((bn (buffer-name)))
                             (when (string-match "^magit\\(.*\\): \\(.*\\)" bn)
                               (let ((kind (string-remove-prefix "-" (match-string 1 bn)))
                                     (what (match-string 2 bn)))
                                 (rename-buffer (if (string-empty-p kind)
                                                    what
                                                  (format "%s: %s" kind what))
                                                t)))))))

(with-eval-after-load 'magit-files
  (transient-replace-suffix 'magit-file-dispatch 'magit-log-trace-definition
    '("t" "Trace" adh-magit-log-trace-region-or-line))
  (transient-replace-suffix 'magit-file-dispatch 'magit-log-buffer-file
    '("l" "Log" adh-magit-log-buffer-file-follow :if-not-derived dired-mode))
  (transient-replace-suffix 'magit-file-dispatch 'magit-blame-addition
    '("b" "Blame" adh-toggle-magit-blame))
  (transient-append-suffix 'magit-file-dispatch ", c"
    '(", R" "Restore" adh-magit-restore-current)))

(with-eval-after-load 'magit-submodule
  (transient-append-suffix 'magit-submodule 'magit-fetch-modules
    '("U" "Update all modules" adh-magit-submodule-update-all))
  (transient-append-suffix 'magit-submodule 'adh-magit-submodule-update-all
    '("!" "Run in each module" adh-magit-submodule-foreach)))

(provide 'adh-magit)

;;; adh-magit.el ends here
