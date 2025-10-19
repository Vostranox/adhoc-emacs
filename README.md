# AdHoc Emacs

This is my personal configuration of Emacs.

The install script will:
- Clone the [gruber-material-dark](https://github.com/Vostranox/gruber-material-dark) theme
- Build and install a custom [fd](https://github.com/Vostranox/fd/tree/feature/simple_sort_by_depth) binary under `~/.emacs.d/opt/fd`
- Load the `init.el` file and install Tree-sitter grammars

---

## Installation

```bash
git clone https://github.com/Vostranox/adhoc-emacs.git ~/.emacs.d
cd ~/.emacs.d
./install/install.sh
```

## Keybindings

The keybindings are defined in adh-keybinds.el and are very personal. Feel free to use it as a reference when setting up your own configuration.
