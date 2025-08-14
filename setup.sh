#!/bin/bash

# Dotfiles setup script
# This script sets up the development environment with all configurations

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source utility functions
source "$SCRIPT_DIR/utils.sh"

echo "Setting up development environment..."

# Detect OS and package manager
detect_os
echo -e "${GREEN}Detected OS: $OS (Family: $OS_FAMILY)${NC}"
echo -e "${GREEN}Package Manager: $PKG_MANAGER${NC}"

# Install essential tools
echo "Installing essential tools..."

# Define packages to install (using generic names)
PACKAGES=(
    xclip
    xdotool
    alacritty
)

# Font packages (will be mapped by get_package_name)
FONT_PACKAGES=(
    fonts-firacode
    fonts-hack
    fonts-jetbrains-mono
    fonts-cascadia-code
)

# Install packages
install_packages "${PACKAGES[@]}" "${FONT_PACKAGES[@]}"

# Create necessary directories
mkdir -p ~/.config/alacritty

# Link configuration files
echo "Linking configuration files..."

# Xmonad
if [ -f "$SCRIPT_DIR/xmonad/xmonad.hs" ]; then
    mkdir -p ~/.xmonad
    create_symlink "$SCRIPT_DIR/xmonad/xmonad.hs" "$HOME/.xmonad/xmonad.hs"
    echo "✓ Xmonad configuration linked"
fi

# Xresources
if [ -f "$SCRIPT_DIR/Xresources" ]; then
    create_symlink "$SCRIPT_DIR/Xresources" "$HOME/.Xresources"
    xrdb -merge ~/.Xresources
    echo "✓ Xresources configuration linked and loaded"
fi

# Zsh configuration
if [ -f "$SCRIPT_DIR/zshrc" ]; then
    create_symlink "$SCRIPT_DIR/zshrc" "$HOME/.zshrc"
    echo "✓ Zsh configuration linked"
elif [ -f "$SCRIPT_DIR/zsh/.zshrc" ]; then
    create_symlink "$SCRIPT_DIR/zsh/.zshrc" "$HOME/.zshrc"
    echo "✓ Zsh configuration linked"
fi

# Alacritty
if [ -f "$SCRIPT_DIR/alacritty/alacritty.toml" ]; then
    create_symlink "$SCRIPT_DIR/alacritty/alacritty.toml" "$HOME/.config/alacritty/alacritty.toml"
    echo "✓ Alacritty configuration linked"
fi

# Xmodmap (if exists)
if [ -f "$SCRIPT_DIR/Xmodmap" ]; then
    create_symlink "$SCRIPT_DIR/Xmodmap" "$HOME/.Xmodmap"
    echo "✓ Xmodmap configuration linked"
fi

echo ""
echo "Setup complete! Please:"
echo "1. Reload your shell: source ~/.zshrc"
echo "2. Restart xmonad: mod4+q (Super+Q)"
echo ""
echo "Clipboard shortcuts:"
echo "  Terminal: pbcopy/pbpaste or copy/paste commands"
echo "  XMonad: Ctrl+Shift+C/V"
echo ""
echo "Your terminal is now set to Alacritty with JetBrains Mono font."