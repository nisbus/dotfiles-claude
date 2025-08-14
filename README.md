# Dotfiles Configuration

Personal configuration files for Emacs, ZSH, XMonad, and related tools.

## Contents

- **emacs/**: Emacs configuration with EXWM support, Zenburn theme, and development tools
- **zsh/**: ZSH configuration (if configured)
- **xmonad/**: XMonad window manager and xmobar configuration
- **xsessions/**: X session initialization scripts for both XMonad and EXWM
- **scripts/**: Installation and utility scripts

## Features

### Emacs Configuration
- Use-package for package management
- Zenburn theme
- EXWM support (conditionally loaded when used as window manager)
- Erlang and JavaScript development support with Flymake
- Smart detection to avoid conflicts when running under other window managers

### X Session Configuration
- Caps Lock mapped to Ctrl
- Separate configurations for XMonad and EXWM sessions
- Clean session initialization

### Window Manager Support
- XMonad configuration (if present)
- EXWM configuration integrated with Emacs
- Xmobar status bar configuration

## Installation

### Quick Install

Clone the repository and run the install script:

```bash
git clone <your-repo-url> ~/dotfiles
cd ~/dotfiles
./install.sh
```

The install script will:
- Back up any existing configuration files
- Create symbolic links to the dotfiles
- Import necessary GPG keys for package verification
- Set up Caps Lock as Ctrl for the current session

### Manual Installation

If you prefer to install configurations selectively:

```bash
# Emacs
ln -s ~/dotfiles/emacs/init.el ~/.emacs.d/init.el

# ZSH (if you have a config)
ln -s ~/dotfiles/zsh/.zshrc ~/.zshrc

# XMonad
mkdir -p ~/.xmonad
ln -s ~/dotfiles/xmonad/xmonad.hs ~/.xmonad/xmonad.hs
ln -s ~/dotfiles/xmonad/.xmobarrc ~/.xmobarrc

# X sessions
ln -s ~/dotfiles/xsessions/xinitrc ~/.xinitrc
ln -s ~/dotfiles/xsessions/xinitrc-exwm ~/.xinitrc-exwm
```

## Prerequisites

### Required Packages

For full functionality, you'll need:

- Emacs (version 27 or higher recommended)
- ZSH (if using ZSH configuration)
- XMonad and xmobar (if using XMonad)
- GPG (for package signature verification)
- Git

### Cross-Platform Support

This repository now supports multiple Linux distributions through automatic OS detection and package manager compatibility. The scripts will automatically detect and use:
- **APT** (Debian, Ubuntu)
- **DNF** (Fedora, RHEL 8+, CentOS Stream)
- **YUM** (older RHEL/CentOS)
- **Pacman** (Arch, Manjaro)
- **Zypper** (openSUSE)
- **Homebrew** (macOS)

### Package Installation Examples

On Fedora:
```bash
sudo dnf install emacs zsh xmonad xmobar git gnupg
```

On Debian/Ubuntu:
```bash
sudo apt install emacs zsh xmonad xmobar git gnupg
```

On Arch:
```bash
sudo pacman -S emacs zsh xmonad xmonad-contrib xmobar git gnupg
```

On openSUSE:
```bash
sudo zypper install emacs zsh xmonad xmobar git gnupg
```

On Guix:
```bash
guix install emacs zsh xmonad xmobar git gnupg
```

## Post-Installation

After installation:

1. **Restart your shell** or run `source ~/.zshrc` if using ZSH
2. **Restart Emacs** to load the new configuration
3. **Recompile XMonad** if needed: `xmonad --recompile`
4. **Log out and back in** for X session changes to take full effect

## Customization

### Switching Window Managers

- For XMonad: Use the standard `.xinitrc`
- For EXWM: Use `.xinitrc-exwm` or configure your display manager to use it

### Emacs Packages

The Emacs configuration will automatically install:
- use-package
- zenburn-theme
- js2-mode
- flymake

Additional packages can be added in `emacs/init.el`.

## Troubleshooting

### GPG Key Issues

If you encounter package signature verification errors in Emacs:

```bash
gpg --keyserver keyserver.ubuntu.com --recv-keys 645357D2883A0966
```

### Caps Lock Not Working

Make sure to run after login:
```bash
setxkbmap -option ctrl:nocaps
```

This is automatically done in the `.xinitrc` files.

## License

These dotfiles are provided as-is for personal use.