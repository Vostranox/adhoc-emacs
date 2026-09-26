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
THEMES_DIR="$EMACS_DIR/themes"

pull_ff() {
    git -C "$1" pull --ff-only || echo "[adh][warning] skipping update of '$1'" >&2
}

mkdir -p "$THEMES_DIR"
if [[ -d "$THEMES_DIR/gruber-material-dark/.git" ]]; then
    pull_ff "$THEMES_DIR/gruber-material-dark"
else
    git clone https://github.com/Vostranox/gruber-material-dark.git "$THEMES_DIR/gruber-material-dark"
fi

mkdir -p "$FD_DIR"
if [[ -d "$FD_DIR/.git" ]]; then
    pull_ff "$FD_DIR"
else
    git clone -b simple_sort_by_depth https://github.com/Vostranox/fd.git "$FD_DIR"
fi

pushd "$FD_DIR" >/dev/null
cargo install --path . --force --locked --root "$FD_DIR"
popd >/dev/null

if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" ]]; then
    EMACS_DIR=$(cygpath -m "$EMACS_DIR")
fi
if [[ ! -d "$EMACS_DIR/elpa" ]]; then
    emacs --batch --eval "(progn
        (load-file \"$EMACS_DIR/init.el\")
        (require 'treesit-auto)
        (treesit-auto-install-all))"
else
    emacs --batch --eval "(progn
        (require 'package)
        (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)
        (package-upgrade-all)
        (package-vc-upgrade-all))"
fi
