# AdHoc Emacs

A coding-focused Emacs configuration built around a batteries included,
loaded on demand design.

Tested and used on GNU/Linux, macOS, and Windows, with startup times under
one second on the tested systems.

## Prerequisites

- Emacs 31 or later
- Git and Cargo
- ripgrep and zoxide
- A Bash-compatible shell to run the installer

## Installation

If you already have a `~/.emacs.d` directory, back it up and move it aside
before cloning.

On Windows, complete the [Windows setup](#windows-setup) before running
these commands.

```bash
git clone https://github.com/Vostranox/adhoc-emacs.git ~/.emacs.d
cd ~/.emacs.d
./install.sh
```

The installer builds and installs a custom
[fd](https://github.com/Vostranox/fd/tree/simple_sort_by_depth) binary
under `~/.emacs.d/opt/fd`.

Package installation and `init.el` generation are handled by `config.el`.
You can recompile the configuration at any time by running `M-x adh-compile-config`.

### Windows Setup

Set the `HOME` environment variable to your Windows user directory
(`%USERPROFILE%`). This configuration assumes `HOME` points to that directory.

Run `install.sh` from a Bash-compatible shell, such as Git Bash.

## Configuration

The generated `init.el` loads the following untracked files if they exist:

- `adh-custom-pre-init.el` — Loaded at the beginning of `init.el`.
- `adh-custom-post-init.el` — Loaded at the end of `init.el`.

Use these files for local customization. Examples are available in `examples/`.

## Keybindings

This configuration uses a custom modal keybinding layout by default.

To define your own keybindings instead, add the following to
`adh-custom-pre-init.el`:

```elisp
(setq adh-use-custom-keybinds nil)
```

Use the following files as references when defining your own keybindings:

- `lisp/adh-meow.el`
- `lisp/adh-keybinds.el`
- `examples/adh-keybindings.el`
