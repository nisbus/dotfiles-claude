#!/bin/bash

# Install XMonad and EXWM session files for Fedora 42
# This script handles Fedora-specific requirements for custom X sessions

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOTFILES_DIR="$(dirname "$SCRIPT_DIR")"

# Source utility functions
source "$DOTFILES_DIR/utils.sh"

echo "Installing X session files for Fedora 42..."

# Detect OS
detect_os

if [ "$OS_FAMILY" != "fedora" ]; then
    echo -e "${YELLOW}Warning: This script is optimized for Fedora. Continuing anyway...${NC}"
fi

# Install XMonad and dependencies
echo -e "${GREEN}Installing XMonad and dependencies...${NC}"
install_packages xmonad xmonad-contrib xmobar dmenu ghc-compiler cabal-install

# Install EXWM dependencies
echo -e "${GREEN}Installing EXWM dependencies...${NC}"
install_packages emacs

# Create user's local xsessions directory (for user-specific sessions)
mkdir -p ~/.local/share/xsessions

# Copy desktop files to user's local xsessions
echo -e "${GREEN}Installing session files...${NC}"
cp "$SCRIPT_DIR/xmonad.desktop" ~/.local/share/xsessions/
cp "$SCRIPT_DIR/exwm.desktop" ~/.local/share/xsessions/

# For system-wide installation (requires sudo)
echo -e "${YELLOW}Would you like to install session files system-wide? (requires sudo) (y/n)${NC}"
read -r INSTALL_SYSTEM
if [[ "$INSTALL_SYSTEM" =~ ^[Yy]$ ]]; then
    sudo cp "$SCRIPT_DIR/xmonad.desktop" /usr/share/xsessions/
    sudo cp "$SCRIPT_DIR/exwm.desktop" /usr/share/xsessions/
    echo -e "${GREEN}Session files installed system-wide.${NC}"
fi

# Make sure xinitrc files are in place and executable
create_symlink "$SCRIPT_DIR/xinitrc" "$HOME/.xinitrc"
create_symlink "$SCRIPT_DIR/xinitrc-exwm" "$HOME/.xinitrc-exwm"
chmod +x "$HOME/.xinitrc"
chmod +x "$HOME/.xinitrc-exwm"

# Compile XMonad configuration if it exists
if [ -f "$HOME/.xmonad/xmonad.hs" ]; then
    echo -e "${GREEN}Compiling XMonad configuration...${NC}"
    xmonad --recompile || echo -e "${YELLOW}Warning: XMonad compilation failed. You may need to fix the configuration.${NC}"
fi

# Update the session files to use proper startup commands for Fedora
echo -e "${GREEN}Updating session files for Fedora compatibility...${NC}"

# Update XMonad desktop file for proper Fedora startup
cat > ~/.local/share/xsessions/xmonad.desktop << 'EOF'
[Desktop Entry]
Name=XMonad
Comment=Lightweight tiling window manager
Exec=xmonad-session
Type=Application
DesktopNames=XMonad
EOF

# Create xmonad-session script
cat > ~/.local/bin/xmonad-session << 'EOF'
#!/bin/bash
# XMonad session startup script for Fedora

# Load Xresources
[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources

# Set keyboard options
setxkbmap -option ctrl:nocaps

# Start XMonad
exec xmonad
EOF

chmod +x ~/.local/bin/xmonad-session

# Update EXWM desktop file
cat > ~/.local/share/xsessions/exwm.desktop << 'EOF'
[Desktop Entry]
Name=EXWM (Emacs Window Manager)
Comment=Emacs X Window Manager
Exec=exwm-session
Type=Application
DesktopNames=EXWM
EOF

# Create exwm-session script
mkdir -p ~/.local/bin
cat > ~/.local/bin/exwm-session << 'EOF'
#!/bin/bash
# EXWM session startup script for Fedora

# Load Xresources
[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources

# Set keyboard options
setxkbmap -option ctrl:nocaps

# Start Emacs with EXWM
exec emacs --eval "(require 'exwm)" --eval "(exwm-enable)"
EOF

chmod +x ~/.local/bin/exwm-session

echo -e "\n${GREEN}Installation complete!${NC}"
echo -e "${YELLOW}Please log out and you should see XMonad and EXWM options in the login screen.${NC}"
echo -e "${YELLOW}Click the gear icon before entering your password to select a session.${NC}"
echo ""
echo "If the sessions don't appear, try:"
echo "  1. Restart GDM: sudo systemctl restart gdm"
echo "  2. Or reboot the system"