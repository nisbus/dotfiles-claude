#!/bin/bash

# Dotfiles Installation Script
# This script sets up Emacs, ZSH, XMonad and related configurations

set -e

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source utility functions
source "$DOTFILES_DIR/utils.sh"

echo "Installing dotfiles from $DOTFILES_DIR"

# Detect OS and package manager
detect_os
echo -e "${GREEN}Detected OS: $OS (Family: $OS_FAMILY)${NC}"
echo -e "${GREEN}Package Manager: $PKG_MANAGER${NC}"

# Install Emacs configuration
echo -e "\n${GREEN}Setting up Emacs configuration...${NC}"
mkdir -p ~/.emacs.d
create_symlink "$DOTFILES_DIR/emacs/init.el" "$HOME/.emacs.d/init.el"

# Import GNU ELPA GPG key for package verification
echo -e "${GREEN}Importing GNU ELPA GPG key...${NC}"
# Try multiple keyservers for better reliability
for keyserver in keys.openpgp.org keyserver.ubuntu.com pgp.mit.edu; do
    gpg --keyserver $keyserver --recv-keys 645357D2883A0966 2>/dev/null && break || true
    gpg --homedir ~/.emacs.d/elpa/gnupg --keyserver $keyserver --recv-keys 645357D2883A0966 2>/dev/null && break || true
done

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

# Install Claude Code
echo -e "\n${YELLOW}Would you like to install Claude Code? (y/n)${NC}"
read -r INSTALL_CLAUDE
if [[ "$INSTALL_CLAUDE" =~ ^[Yy]$ ]]; then
    if [ -f "$DOTFILES_DIR/claude/install-claude.sh" ]; then
        bash "$DOTFILES_DIR/claude/install-claude.sh"
        
        # Copy Claude configuration if it exists
        if [ -d "$DOTFILES_DIR/claude/dot-claude" ]; then
            echo -e "${GREEN}Setting up Claude configuration...${NC}"
            mkdir -p ~/.claude
            cp -r "$DOTFILES_DIR/claude/dot-claude/"* ~/.claude/ 2>/dev/null || true
            echo -e "${GREEN}Claude configuration copied.${NC}"
        fi
    fi
fi

# Optional: Install Barrier for keyboard/mouse sharing
echo -e "\n${YELLOW}Would you like to install Barrier for keyboard/mouse sharing? (y/n)${NC}"
read -r INSTALL_BARRIER
if [[ "$INSTALL_BARRIER" =~ ^[Yy]$ ]]; then
    if [ -f "$DOTFILES_DIR/barrier/barrier-setup.sh" ]; then
        bash "$DOTFILES_DIR/barrier/barrier-setup.sh"
    fi
fi

# Optional: Install development environment
echo -e "\n${YELLOW}Would you like to install the complete development environment?${NC}"
echo -e "${YELLOW}This includes: Oh My Zsh, Make, Docker, Go, Erlang, Node.js, and dev tools (y/n)${NC}"
read -r INSTALL_DEV
if [[ "$INSTALL_DEV" =~ ^[Yy]$ ]]; then
    if [ -f "$DOTFILES_DIR/dev-setup.sh" ]; then
        bash "$DOTFILES_DIR/dev-setup.sh"
    fi
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