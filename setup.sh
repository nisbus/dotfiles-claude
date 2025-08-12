#!/bin/bash

# Dotfiles setup script
# This script sets up the development environment with all configurations

echo "Setting up development environment..."

# Update package lists
sudo apt-get update

# Install essential tools
echo "Installing essential tools..."
sudo apt-get install -y \
    xclip \
    xdotool \
    alacritty \
    fonts-firacode \
    fonts-hack \
    fonts-jetbrains-mono \
    fonts-cascadia-code

# Create necessary directories
mkdir -p ~/.config/alacritty

# Link configuration files
echo "Linking configuration files..."

# Xmonad
if [ -f ~/dotfiles/xmonad/xmonad.hs ]; then
    mkdir -p ~/.xmonad
    ln -sf ~/dotfiles/xmonad/xmonad.hs ~/.xmonad/xmonad.hs
    echo "✓ Xmonad configuration linked"
fi

# Xresources
if [ -f ~/dotfiles/Xresources ]; then
    ln -sf ~/dotfiles/Xresources ~/.Xresources
    xrdb -merge ~/.Xresources
    echo "✓ Xresources configuration linked and loaded"
fi

# Zsh configuration
if [ -f ~/dotfiles/zshrc ]; then
    ln -sf ~/dotfiles/zshrc ~/.zshrc
    echo "✓ Zsh configuration linked"
fi

# Alacritty
if [ -f ~/dotfiles/alacritty/alacritty.toml ]; then
    ln -sf ~/dotfiles/alacritty/alacritty.toml ~/.config/alacritty/alacritty.toml
    echo "✓ Alacritty configuration linked"
fi

# Xmodmap (if exists)
if [ -f ~/dotfiles/Xmodmap ]; then
    ln -sf ~/dotfiles/Xmodmap ~/.Xmodmap
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