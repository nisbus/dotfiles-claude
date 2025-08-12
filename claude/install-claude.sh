#!/bin/bash

# Claude Code Installation Script
# This script installs Claude Code and sets up user configuration

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}Installing Claude Code...${NC}"

# Check if npm is installed
if ! command -v npm &> /dev/null; then
    echo -e "${YELLOW}npm is not installed. Installing Node.js and npm...${NC}"
    
    # Detect OS and install accordingly
    if [ -f /etc/debian_version ]; then
        # Debian/Ubuntu
        curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -
        sudo apt-get install -y nodejs
    elif [ -f /etc/redhat-release ]; then
        # RHEL/CentOS/Fedora
        curl -fsSL https://rpm.nodesource.com/setup_lts.x | sudo bash -
        sudo yum install -y nodejs
    elif [ "$(uname)" == "Darwin" ]; then
        # macOS
        if ! command -v brew &> /dev/null; then
            echo -e "${RED}Homebrew is not installed. Please install Homebrew first.${NC}"
            exit 1
        fi
        brew install node
    else
        echo -e "${RED}Unsupported OS. Please install Node.js manually.${NC}"
        exit 1
    fi
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