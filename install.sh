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
SITE_LISP_DIR="$EMACS_DIR/site-lisp"

clone_or_update() {
    local url="$1"
    local dir
    dir="$SITE_LISP_DIR/$(basename "$url" .git)"
    if [[ -d "$dir/.git" ]]; then
        git -C "$dir" pull --ff-only
    else
        git clone "$url" "$dir"
    fi
}

mkdir -p "$THEMES_DIR"
if [[ -d "$THEMES_DIR/gruber-material-dark/.git" ]]; then
    git -C "$THEMES_DIR/gruber-material-dark" pull --ff-only
else
    git clone https://github.com/Vostranox/gruber-material-dark.git "$THEMES_DIR/gruber-material-dark"
fi

mkdir -p "$FD_DIR"
if [[ -d "$FD_DIR/.git" ]]; then
    git -C "$FD_DIR" pull --ff-only
else
    git clone -b simple_sort_by_depth https://github.com/Vostranox/fd.git "$FD_DIR"
fi

pushd "$FD_DIR" >/dev/null
cargo install --path . --force --locked --root "$FD_DIR"
popd >/dev/null

mkdir -p "$SITE_LISP_DIR"
clone_or_update "https://github.com/Vostranox/slang-ts-mode.git"
clone_or_update "https://github.com/Vostranox/hlsl-ts-mode.git"

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
        (package-refresh-contents)
        (package-upgrade-all t))"
fi
