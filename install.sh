#!/usr/bin/env bash
set -euo pipefail

for cmd in git cargo emacs; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "[adh][error] '$cmd' not found in PATH." >&2
        exit 1
    fi
done

for cmd in rg zoxide; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "[adh][warning] '$cmd' not found in PATH." >&2
    fi
done

EMACS_DIR="$HOME/.emacs.d"
FD_DIR="$EMACS_DIR/opt/fd"

if [[ -d "$FD_DIR/.git" ]]; then
    git -C "$FD_DIR" pull --ff-only || echo "[adh][warning] skipping update of '$FD_DIR'" >&2
else
    git clone -b simple_sort_by_depth https://github.com/Vostranox/fd.git "$FD_DIR"
fi
(cd "$FD_DIR" && cargo install --path . --force --locked --root "$FD_DIR")

if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" ]]; then
    EMACS_DIR=$(cygpath -m "$EMACS_DIR")
fi
if [[ ! -d "$EMACS_DIR/elpa" ]]; then
    emacs --batch --eval "(progn
        (load-file \"$EMACS_DIR/early-init.el\")
        (setq gc-cons-threshold (* 64 1024 1024))
        (load-file \"$EMACS_DIR/config.el\")
        (when adh--init-errors-p
          (error \"[adh] Configuration setup failed; fix the reported errors before building\"))
        (let ((user-init-file custom-file))
          (package--save-selected-packages package-selected-packages))
        (adh-treesit-ensure-grammars))"
else
    emacs --batch --eval "(progn
        (when (featurep 'native-compile)
          (startup-redirect-eln-cache \"$EMACS_DIR/var/eln-cache/\"))
        (load \"$EMACS_DIR/lisp/adh-options.el\")
        (with-demoted-errors \"[adh][error] adh-custom-pre-init.el: %S\"
          (load \"$EMACS_DIR/adh-custom-pre-init.el\" t))
        (require 'package)
        (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)
        (package-upgrade-all)
        (package-vc-upgrade-all)
        (while (and vc-post-command-functions (seq-some #'process-command (process-list)))
          (accept-process-output nil 0.1)))"
fi

emacs --batch --eval "(progn
    (load-file \"$EMACS_DIR/early-init.el\")
    (let ((proc (adh-compile-config t)))
      (while (process-live-p proc)
        (accept-process-output proc 0.1))
      (unless (zerop (process-exit-status proc))
        (princ (with-current-buffer (process-buffer proc) (buffer-string)))
        (message \"[adh][error] Building init failed; run M-x adh-compile-config to retry\"))
      (kill-emacs (process-exit-status proc))))"
