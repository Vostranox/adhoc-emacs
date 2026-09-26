# AdHoc Emacs

Emacs configuration designed for coding. Batteries included on demand (LSP, autocompletion).
Tested and used on GNU/Linux, macOS, and Windows.

## Prerequisites

- **Requires**: Emacs (31+), Git, and Cargo.
- **Dependencies**: Ripgrep, Zoxide

## Configuration

For local customization, `init.el` will automatically load the following untracked files if they exist. Examples are provided in `examples/`.
- `adh-custom-pre-init.el` – Evaluated at the beginning of `init.el`
- `adh-custom-post-init.el` – Evaluated at the end of `init.el`

## Keybindings

This setup defaults to a custom modal layout. To write your own keybindings from scratch, set `adh-use-custom-keybinds` to `nil` in `adh-custom-pre-init.el`. You can use these files as a reference:
- `lisp/adh-meow.el`
- `lisp/adh-keybinds.el`
- `examples/adh-keybindings.el`

## Installation

The install script will:
- Build and install a custom [fd](https://github.com/Vostranox/fd/tree/simple_sort_by_depth) binary under `~/.emacs.d/opt/fd`
- On first run, load `init.el` to install all packages (including the [gruber-material-dark](https://github.com/Vostranox/gruber-material-dark) theme) and the Tree-sitter grammars in `adh-treesit-ensured-langs`; on later runs, upgrade installed packages, including VC ones
- Byte-compile `lisp/`

```bash
git clone https://github.com/Vostranox/adhoc-emacs.git ~/.emacs.d
cd ~/.emacs.d
./install.sh
```

### Windows Setup
To use your standard Windows User directory as your Emacs home, set the `HOME` environment variable to `%USERPROFILE%` (equivalent to `~/` in Git Bash). This configuration assumes you have this variable set. Note: You will need to run the install.sh script using a bash-compatible terminal like Git Bash.

### Tree-sitter
Grammars live in `var/treesit/`. Those not in `adh-treesit-ensured-langs` are installed the first time you open a file of their language; `M-x adh-treesit-ensure-grammars` installs any missing ones from the list. Watch out for version-mismatch errors during installation. If you encounter them, you will likely need to either upgrade your Emacs version or downgrade the specific Tree-sitter grammars.
