#!/usr/bin/env bash
#
# Usage:
#   ./install-emacs-arch.sh [-j N] [-p PREFIX] [-s SRC_DIR] [-w]
#     -j N       parallel make jobs        (default: all cores)
#     -p PREFIX  install directory         (default: ~/opt/emacs)
#     -s SRC_DIR source checkout directory (default: ~/probe/emacs)
#     -w         Wayland-native build (pgtk) instead of GTK3 under X

set -euo pipefail

PREFIX="${PREFIX:-$HOME/opt/emacs}"
SRC_DIR="${SRC_DIR:-$HOME/probe/emacs}"
JOBS="${JOBS:-$(nproc)}"
PGTK="${PGTK:-0}"

usage() {
    echo "usage: ${0##*/} [-j N] [-p PREFIX] [-s SRC_DIR] [-w]" >&2
    exit 2
}

while getopts ":j:p:s:wh" opt; do
    case $opt in
        j) [[ $OPTARG =~ ^[0-9]+$ ]] || usage
           JOBS=$OPTARG ;;
        p) PREFIX=$OPTARG ;;
        s) SRC_DIR=$OPTARG ;;
        w) PGTK=1 ;;
        h|*) usage ;;
    esac
done
shift $((OPTIND - 1))
[[ $# -eq 0 ]] || usage

if [[ "$PGTK" == "1" ]]; then
    TOOLKIT_FLAG="--with-pgtk"
else
    TOOLKIT_FLAG="--with-x-toolkit=gtk3"
fi

echo "[emacs-build] Installing build dependencies (sudo)..."
sudo pacman -S --needed \
    base-devel \
    git \
    cairo \
    fontconfig \
    freetype2 \
    giflib \
    gmp \
    gnutls \
    gtk3 \
    harfbuzz \
    imagemagick \
    libgccjit \
    libjpeg-turbo \
    libpng \
    librsvg \
    libtiff \
    libwebp \
    libxml2 \
    libxpm \
    nettle \
    p11-kit \
    sqlite \
    texinfo \
    tree-sitter

if [[ -d "$SRC_DIR/.git" ]]; then
    git -C "$SRC_DIR" pull --ff-only
else
    git clone --depth 1 https://github.com/emacs-mirror/emacs "$SRC_DIR"
fi

cd "$SRC_DIR"
./autogen.sh
mkdir -p build
cd build
../configure --prefix="$PREFIX" \
    "$TOOLKIT_FLAG" \
    --with-native-compilation=aot \
    --with-gnutls \
    --without-dbus \
    --without-pop \
    --with-xpm \
    --with-imagemagick \
    --with-tree-sitter

make -j"$JOBS" bootstrap
make install

echo "[emacs-build] Installed to $PREFIX"
