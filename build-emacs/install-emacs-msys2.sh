#!/usr/bin/env bash
#
# Run this inside the "MSYS2 UCRT64" shell.
#
# Usage:
#   ./install-emacs-msys2.sh [-j N] [-p PREFIX] [-s SRC_DIR]
#     -j N       parallel make jobs        (default: all cores)
#     -p PREFIX  install directory         (default: ~/opt/emacs)
#     -s SRC_DIR source checkout directory (default: ~/probe/emacs)

set -euo pipefail

PREFIX="${PREFIX:-$HOME/opt/emacs}"
SRC_DIR="${SRC_DIR:-$HOME/probe/emacs}"
JOBS="${JOBS:-$(nproc)}"

usage() {
    echo "usage: ${0##*/} [-j N] [-p PREFIX] [-s SRC_DIR]" >&2
    exit 2
}

while getopts ":j:p:s:h" opt; do
    case $opt in
        j) [[ $OPTARG =~ ^[0-9]+$ ]] || usage
           JOBS=$OPTARG ;;
        p) PREFIX=$OPTARG ;;
        s) SRC_DIR=$OPTARG ;;
        h|*) usage ;;
    esac
done
shift $((OPTIND - 1))
[[ $# -eq 0 ]] || usage

if [[ "${MSYSTEM:-}" != "UCRT64" ]]; then
    echo "[emacs-build][error] Run this from the MSYS2 UCRT64 shell (MSYSTEM=${MSYSTEM:-unset})." >&2
    exit 1
fi

echo "[emacs-build] Updating MSYS2 (rerun this script if the terminal restarts)..."
pacman -Syuu --noconfirm

echo "[emacs-build] Installing build dependencies..."
pacman -S --needed --noconfirm \
    autoconf \
    autogen \
    automake \
    automake-wrapper \
    git \
    libidn-devel \
    libltdl \
    libnettle-devel \
    libopenssl \
    libp11-kit-devel \
    libtasn1-devel \
    libunistring \
    make \
    mingw-w64-ucrt-x86_64-toolchain \
    mingw-w64-ucrt-x86_64-bzip2 \
    mingw-w64-ucrt-x86_64-cairo \
    mingw-w64-ucrt-x86_64-crt-git \
    mingw-w64-ucrt-x86_64-expat \
    mingw-w64-ucrt-x86_64-fontconfig \
    mingw-w64-ucrt-x86_64-freetype \
    mingw-w64-ucrt-x86_64-gcc \
    mingw-w64-ucrt-x86_64-gcc-libs \
    mingw-w64-ucrt-x86_64-gdk-pixbuf2 \
    mingw-w64-ucrt-x86_64-gettext \
    mingw-w64-ucrt-x86_64-giflib \
    mingw-w64-ucrt-x86_64-glib2 \
    mingw-w64-ucrt-x86_64-gmp \
    mingw-w64-ucrt-x86_64-gnutls \
    mingw-w64-ucrt-x86_64-harfbuzz \
    mingw-w64-ucrt-x86_64-headers-git \
    mingw-w64-ucrt-x86_64-imagemagick \
    mingw-w64-ucrt-x86_64-libgccjit \
    mingw-w64-ucrt-x86_64-libiconv \
    mingw-w64-ucrt-x86_64-libidn2 \
    mingw-w64-ucrt-x86_64-libjpeg-turbo \
    mingw-w64-ucrt-x86_64-libpng \
    mingw-w64-ucrt-x86_64-librsvg \
    mingw-w64-ucrt-x86_64-sqlite3 \
    mingw-w64-ucrt-x86_64-libtree-sitter \
    mingw-w64-ucrt-x86_64-libtiff \
    mingw-w64-ucrt-x86_64-libunistring \
    mingw-w64-ucrt-x86_64-libxml2 \
    mingw-w64-ucrt-x86_64-nettle \
    mingw-w64-ucrt-x86_64-p11-kit \
    mingw-w64-ucrt-x86_64-winpthreads-git \
    mingw-w64-ucrt-x86_64-xpm-nox \
    mingw-w64-ucrt-x86_64-xz \
    mingw-w64-ucrt-x86_64-zlib \
    mingw-w64-ucrt-x86_64-jbigkit \
    pkgconf \
    texinfo

# CRLF checkouts break the build.
git config --global core.autocrlf false

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
echo "[emacs-build] Launch with emacs.bat (adjust MSYS2_ROOT/EMACS_HOME inside it first)."
