;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'adh-functions)

(defvar adh--shell-prompt-dir nil
  "When set, adh labels compile and shell prompts with this directory.")

(declare-function project-try-vc "project" (dir))

(defun adh--project-try (&optional dir)
  "Project.el backend: DIR's project, Git-backed if its root has a .git entry."
  (when-let* ((root (adh--get-project-dir dir)))
    (or (and (file-exists-p (expand-file-name ".git" root))
             (let ((vc-handled-backends '(Git)))
               (project-try-vc root)))
        (cons 'transient (expand-file-name root)))))

(defun adh--run-with-region (command dir)
  "Run COMMAND from DIR, starting its prompt with the active region, if any."
  (let* ((region (and (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))))
         (adh--command-origin-dir default-directory)
         (default-directory dir)
         (adh--shell-prompt-dir dir))
    (minibuffer-with-setup-hook
        (lambda ()
          (when region
            (delete-minibuffer-contents)
            (insert region)))
      (call-interactively command))))

(defun adh-shell-command-dir-pivot (&optional dir)
  "Rerun the current compile or shell command prompt in DIR, or one read."
  (interactive)
  (let ((input (minibuffer-contents-no-properties))
        (origin (adh--origin-dir))
        (command (pcase (minibuffer-prompt)
                   ((rx bos "Compile" (or " command: " " (")) #'compile)
                   ((rx bos "Async shell" (or " command: " " command in " " (")) #'async-shell-command)
                   ((rx bos "Shell" (or " command: " " command in " " (")) #'shell-command)
                   (_ (user-error "No directory pivot for this prompt")))))
    (adh--minibuffer-pivot-call
     (lambda ()
       (let* ((adh--command-origin-dir origin)
              (default-directory (or dir
                                     (let ((use-dialog-box nil))
                                       (expand-file-name (read-directory-name "Run in: " origin nil t)))))
              (adh--shell-prompt-dir default-directory))
         (minibuffer-with-setup-hook
             (lambda ()
               (delete-minibuffer-contents)
               (insert input))
           (call-interactively command)))))))

(defun adh-shell-command-root-pivot ()
  "Rerun the current compile or shell command prompt at the project root.
When it already runs there, rerun it in the directory it started from."
  (interactive)
  (let* ((origin (adh--origin-dir))
         (root (adh--get-project-dir origin))
         (dir (if (and root (not (file-equal-p default-directory root))) root origin)))
    (if (file-equal-p dir default-directory)
        (user-error (if root "Already at the project root" "Not in a project"))
      (adh-shell-command-dir-pivot dir))))

(defun adh-compile-region ()
  "Compile from `default-directory', starting with the active region."
  (interactive)
  (adh--run-with-region #'compile default-directory))

(defun adh-async-shell-command-region ()
  "Run an async shell command from `default-directory', with the active region."
  (interactive)
  (adh--run-with-region #'async-shell-command default-directory))

(defun adh-project-compile-region ()
  "Compile from the project root, starting with the active region."
  (interactive)
  (adh--run-with-region #'compile (or (adh--get-project-dir) default-directory)))

(defun adh-project-async-shell-command-region ()
  "Run an async shell command from the project root, with the active region."
  (interactive)
  (adh--run-with-region #'async-shell-command (or (adh--get-project-dir) default-directory)))

(defun adh--dir-label (dir)
  "Name DIR like consult prompts do: \"Project NAME\" at a root, else the path."
  (let ((root (adh--get-project-dir dir)))
    (if (and root (file-equal-p root dir))
        (concat "Project " (file-name-nondirectory (directory-file-name root)))
      (directory-file-name (abbreviate-file-name dir)))))

(define-advice read-shell-command (:filter-args (args) adh-dir-label)
  "Label the prompt with `adh--shell-prompt-dir' when an adh command sets it."
  (when-let* ((dir adh--shell-prompt-dir)
              (base (pcase (car args)
                      ("Compile command: " "Compile")
                      ("Async shell command: " "Async shell")
                      ("Shell command: " "Shell"))))
    (setcar args (format "%s (%s): " base (adh--dir-label dir))))
  args)

(use-package project
  :ensure nil :defer t
  :config
  (setq project-find-functions #'adh--project-try)
  (cl-defmethod project-external-roots ((_project (head vc))) nil))

(provide 'adh-project)

;;; adh-project.el ends here
