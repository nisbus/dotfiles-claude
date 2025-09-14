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
    xrandr
    arandr
    autorandr
    picom
    numlockx
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
mkdir -p ~/.config/picom

# Link configuration files
echo "Linking configuration files..."

# Xmonad
if [ -f "$SCRIPT_DIR/xmonad/xmonad.hs" ]; then
    mkdir -p ~/.xmonad
    create_symlink "$SCRIPT_DIR/xmonad/xmonad.hs" "$HOME/.xmonad/xmonad.hs"
    echo "✓ Xmonad configuration linked"
fi

# Xmobar
if [ -f "$SCRIPT_DIR/xmobar/configure-xmobar.sh" ]; then
    # Auto-detect network interfaces and generate xmobar config
    "$SCRIPT_DIR/xmobar/configure-xmobar.sh"
    create_symlink "$SCRIPT_DIR/xmobar/xmobarrc" "$HOME/.xmobarrc"
    echo "✓ Xmobar configuration generated and linked"
elif [ -f "$SCRIPT_DIR/xmobar/xmobarrc" ]; then
    create_symlink "$SCRIPT_DIR/xmobar/xmobarrc" "$HOME/.xmobarrc"
    echo "✓ Xmobar configuration linked"
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

# Picom compositor
if [ -f "$SCRIPT_DIR/picom/picom.conf" ]; then
    create_symlink "$SCRIPT_DIR/picom/picom.conf" "$HOME/.config/picom/picom.conf"
    echo "✓ Picom configuration linked"
fi

# Xmodmap (if exists)
if [ -f "$SCRIPT_DIR/Xmodmap" ]; then
    create_symlink "$SCRIPT_DIR/Xmodmap" "$HOME/.Xmodmap"
    echo "✓ Xmodmap configuration linked"
fi

# Setup monitor configuration if script exists
if [ -f "$SCRIPT_DIR/monitor/setup-monitors.sh" ]; then
    echo ""
    echo "Setting up monitor configuration..."
    bash "$SCRIPT_DIR/monitor/setup-monitors.sh"
fi

# Setup screenshot functionality if script exists
if [ -f "$SCRIPT_DIR/screenshot/setup-screenshot.sh" ]; then
    echo ""
    echo "Setting up screenshot functionality..."
    bash "$SCRIPT_DIR/screenshot/setup-screenshot.sh"
fi

# Network management setup - automatically configure both approaches
echo ""
echo "=== Network Management Setup ==="
echo "Setting up both NetworkManager dmenu (Super+n) and system tray (GUI icons)..."

# Setup networkmanager_dmenu
if [ -f "$SCRIPT_DIR/networkmanager/setup-networkmanager-dmenu.sh" ]; then
    echo "Setting up NetworkManager dmenu interface..."
    bash "$SCRIPT_DIR/networkmanager/setup-networkmanager-dmenu.sh"
fi

# Setup stalonetray with nm-applet
if [ -f "$SCRIPT_DIR/stalonetray/setup-stalonetray.sh" ]; then
    echo "Setting up stalonetray with nm-applet..."
    bash "$SCRIPT_DIR/stalonetray/setup-stalonetray.sh"
    # Use the tray-enabled startup script
    if [ -f "$SCRIPT_DIR/xsessions/xmonad-start-tray" ]; then
        create_symlink "$SCRIPT_DIR/xsessions/xmonad-start-tray" "$HOME/.xmonad/xmonad-start"
        echo "✓ XMonad configured with both network management options"
    fi
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
echo "Monitor management:"
echo "  - Monitors will auto-detect when connected/disconnected"
echo "  - Use 'autorandr' command to manually switch profiles"
echo ""
echo "Your terminal is now set to Alacritty with JetBrains Mono font."