#!/bin/bash

# Claude Code Installation Script
# This script installs Claude Code and sets up user configuration

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOTFILES_DIR="$(dirname "$SCRIPT_DIR")"

# Source utility functions
source "$DOTFILES_DIR/utils.sh"

echo -e "${GREEN}Installing Claude Code...${NC}"

# Check if npm is installed
if ! command -v npm &> /dev/null; then
    echo -e "${YELLOW}npm is not installed. Installing Node.js and npm...${NC}"
    
    # Detect OS and package manager
    detect_os
    
    # Install Node.js based on package manager
    case "$PKG_MANAGER" in
        apt)
            curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -
            install_packages nodejs
            ;;
        dnf)
            curl -fsSL https://rpm.nodesource.com/setup_lts.x | sudo bash -
            install_packages nodejs
            ;;
        yum)
            curl -fsSL https://rpm.nodesource.com/setup_lts.x | sudo bash -
            install_packages nodejs
            ;;
        brew)
            brew install node
            ;;
        pacman)
            install_packages nodejs npm
            ;;
        zypper)
            install_packages nodejs npm
            ;;
        *)
            echo -e "${RED}Unsupported package manager: $PKG_MANAGER. Please install Node.js manually.${NC}"
            exit 1
            ;;
    esac
fi

# Setup npm to use user directory for global packages
echo -e "${GREEN}Configuring npm for user-level global packages...${NC}"
mkdir -p ~/.npm-global
npm config set prefix '~/.npm-global'

# Add npm global bin to PATH if not already there
if ! grep -q ".npm-global/bin" ~/.bashrc 2>/dev/null; then
    echo 'export PATH=~/.npm-global/bin:$PATH' >> ~/.bashrc
fi

if ! grep -q ".npm-global/bin" ~/.zshrc 2>/dev/null; then
    echo 'export PATH=~/.npm-global/bin:$PATH' >> ~/.zshrc
fi

# Export for current session
export PATH=~/.npm-global/bin:$PATH

# Install Claude Code
echo -e "${GREEN}Installing Claude Code...${NC}"
npm install -g @anthropic-ai/claude-code

# Verify installation
if command -v claude &> /dev/null; then
    VERSION=$(claude --version)
    echo -e "${GREEN}Claude Code installed successfully! Version: $VERSION${NC}"
else
    echo -e "${RED}Claude Code installation failed. Please check the error messages above.${NC}"
    exit 1
fi

echo -e "${GREEN}Claude Code installation complete!${NC}"
echo -e "${YELLOW}Note: You may need to restart your shell or run 'source ~/.bashrc' (or ~/.zshrc) for PATH changes.${NC}"