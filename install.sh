#!/bin/bash

# Dotfiles Installation Script
# This script sets up Emacs, ZSH, XMonad and related configurations

set -e

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Installing dotfiles from $DOTFILES_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to create backup of existing files
backup_file() {
    if [ -f "$1" ]; then
        echo -e "${YELLOW}Backing up existing $1 to $1.backup${NC}"
        cp "$1" "$1.backup"
    fi
}

# Function to create symlink
create_symlink() {
    local source="$1"
    local target="$2"
    
    if [ -e "$target" ] || [ -L "$target" ]; then
        backup_file "$target"
        rm -f "$target"
    fi
    
    echo -e "${GREEN}Creating symlink: $target -> $source${NC}"
    ln -s "$source" "$target"
}

# Install Emacs configuration
echo -e "\n${GREEN}Setting up Emacs configuration...${NC}"
mkdir -p ~/.emacs.d
create_symlink "$DOTFILES_DIR/emacs/init.el" "$HOME/.emacs.d/init.el"

# Import GNU ELPA GPG key for package verification
echo -e "${GREEN}Importing GNU ELPA GPG key...${NC}"
gpg --keyserver keyserver.ubuntu.com --recv-keys 645357D2883A0966 2>/dev/null || true
gpg --homedir ~/.emacs.d/elpa/gnupg --keyserver keyserver.ubuntu.com --recv-keys 645357D2883A0966 2>/dev/null || true

# Install ZSH configuration
if [ -f "$DOTFILES_DIR/zsh/.zshrc" ]; then
    echo -e "\n${GREEN}Setting up ZSH configuration...${NC}"
    create_symlink "$DOTFILES_DIR/zsh/.zshrc" "$HOME/.zshrc"
fi

# Install XMonad configuration
if [ -f "$DOTFILES_DIR/xmonad/xmonad.hs" ]; then
    echo -e "\n${GREEN}Setting up XMonad configuration...${NC}"
    mkdir -p ~/.xmonad
    create_symlink "$DOTFILES_DIR/xmonad/xmonad.hs" "$HOME/.xmonad/xmonad.hs"
fi

# Install xmobar configuration
if [ -f "$DOTFILES_DIR/xmonad/.xmobarrc" ]; then
    echo -e "${GREEN}Setting up xmobar configuration...${NC}"
    create_symlink "$DOTFILES_DIR/xmonad/.xmobarrc" "$HOME/.xmobarrc"
fi

# Install X session files
echo -e "\n${GREEN}Setting up X session files...${NC}"
create_symlink "$DOTFILES_DIR/xsessions/xinitrc" "$HOME/.xinitrc"
create_symlink "$DOTFILES_DIR/xsessions/xinitrc-exwm" "$HOME/.xinitrc-exwm"

# Make sure X session files are executable
chmod +x "$HOME/.xinitrc"
chmod +x "$HOME/.xinitrc-exwm"

# Set Caps Lock to Ctrl (for current session if X is running)
if [ -n "$DISPLAY" ]; then
    echo -e "\n${GREEN}Setting Caps Lock to Ctrl for current session...${NC}"
    setxkbmap -option ctrl:nocaps
fi

echo -e "\n${GREEN}Installation complete!${NC}"
echo -e "${YELLOW}Note: You may need to:${NC}"
echo "  1. Restart your shell or run 'source ~/.zshrc' for ZSH changes"
echo "  2. Restart Emacs for configuration changes"
echo "  3. Recompile XMonad with 'xmonad --recompile' if you made changes"
echo "  4. Log out and back in for X session changes to take full effect"
echo ""
echo "The Caps Lock key has been mapped to Ctrl for this session."
echo "This mapping will persist across reboots via .xinitrc"