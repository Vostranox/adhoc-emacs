#!/usr/bin/env bash
#
# Usage:
#   ./install-emacs-macos.sh [-j N] [-a APP_DIR] [-s SRC_DIR]
#     -j N       parallel make jobs        (default: all cores)
#     -a APP_DIR where Emacs.app is copied (default: /Applications)
#     -s SRC_DIR source checkout directory (default: ~/probe/emacs)

set -euo pipefail

SRC_DIR="${SRC_DIR:-$HOME/probe/emacs}"
APP_DIR="${APP_DIR:-/Applications}"
JOBS="${JOBS:-$(sysctl -n hw.ncpu)}"

usage() {
    echo "usage: ${0##*/} [-j N] [-a APP_DIR] [-s SRC_DIR]" >&2
    exit 2
}

while getopts ":j:a:s:h" opt; do
    case $opt in
        j) [[ $OPTARG =~ ^[0-9]+$ ]] || usage
           JOBS=$OPTARG ;;
        a) APP_DIR=$OPTARG ;;
        s) SRC_DIR=$OPTARG ;;
        h|*) usage ;;
    esac
done
shift $((OPTIND - 1))
[[ $# -eq 0 ]] || usage

if ! command -v brew &>/dev/null; then
    echo "[emacs-build][error] Homebrew not found (https://brew.sh)." >&2
    exit 1
fi

echo "[emacs-build] Installing build dependencies..."
brew install \
    autoconf \
    automake \
    gcc \
    libgccjit \
    giflib \
    gmp \
    gnutls \
    imagemagick \
    jpeg-turbo \
    libpng \
    librsvg \
    libtiff \
    libxml2 \
    nettle \
    p11-kit \
    pkgconf \
    texinfo \
    tree-sitter \
    webp

if [[ -d "$SRC_DIR/.git" ]]; then
    git -C "$SRC_DIR" pull --ff-only
else
    git clone --depth 1 https://github.com/emacs-mirror/emacs "$SRC_DIR"
fi

cd "$SRC_DIR"
./autogen.sh
mkdir -p build
cd build
../configure \
    --with-ns \
    --with-native-compilation=aot \
    --with-gnutls \
    --without-dbus \
    --without-pop \
    --with-imagemagick \
    --with-tree-sitter

make -j"$JOBS" bootstrap
make install

rm -rf "$APP_DIR/Emacs.app"
cp -R nextstep/Emacs.app "$APP_DIR/"

echo "[emacs-build] Installed $APP_DIR/Emacs.app"
