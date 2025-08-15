# Dotfiles Configuration

Personal configuration files for Emacs, ZSH, XMonad, and related tools.

## Attribution

This configuration incorporates themes and design elements from:
- **[gh0stzk/dotfiles](https://github.com/gh0stzk/dotfiles)** - Theme system, EWW widget designs, and color schemes for Cristina theme
- **[Axarva/dotfiles-2.0](https://github.com/Axarva/dotfiles-2.0)** - Additional theme configurations and styling concepts

Special thanks to these developers for their excellent work and inspiration.

## Contents

- **emacs/**: Emacs configuration with EXWM support, Zenburn theme, and development tools
- **zsh/**: ZSH configuration (if configured)
- **xmonad/**: XMonad window manager and xmobar configuration
- **xsessions/**: X session initialization scripts for both XMonad and EXWM
- **scripts/**: Installation and utility scripts

## Features

### Theme System
- Dynamic theme switching with Super+t
- Two beautiful themes: Cristina (dark purple) and Silvia
- Automatic color synchronization across all components
- Theme-aware notifications with Dunst
- GTK application theming support

### EWW Widget System  
- Volume, brightness, and terminal opacity controls
- System monitoring (CPU, RAM, battery)
- Fully theme-aware colors
- Toggle with Super+i

### Window Management
- XMonad tiling window manager
- Transparency support via Picom compositor
- Scratchpad terminal (Super+`)
- Lock screen with i3lock (Super+Shift+l)
- Rofi cheatsheet (Super+F1)

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

## Installation

### Prerequisites

- Fedora/RHEL/Debian/Ubuntu/Arch Linux
- XMonad window manager
- Git

### Quick Install

Clone the repository and run the install scripts:

```bash
git clone <your-repo-url> ~/dotfiles-claude
cd ~/dotfiles-claude

# Install base dotfiles
./install.sh

# Install XMonad features (themes, widgets, compositor, etc.)
./install-features.sh
```

The installation will:
- Install required packages (picom, dunst, rofi, EWW, etc.)
- Set up configuration files
- Configure themes and widgets
- Create all necessary directories
- Set up GTK theming

## Key Bindings

### Essential Shortcuts
- `Super+t` - Theme selector (switch between Cristina and Silvia)
- `Super+i` - Toggle EWW widgets (volume/brightness/opacity/system info)
- `Super+F1` - Show keybindings cheatsheet
- `Super+Shift+l` - Lock screen with wallpaper
- `Super+Shift+z` - Lock screen with blur
- `Super+`` ` - Scratchpad terminal
- `Super+Shift+p` - Power menu
- `Super+q` - Restart XMonad
- `Super+Shift+q` - Quit XMonad

### Window Management
- `Super+Space` - Next layout
- `Super+Tab` - Focus next window
- `Super+Shift+c` - Close window
- `Super+Return` - Swap with master

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