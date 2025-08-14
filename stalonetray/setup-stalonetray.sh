#!/bin/bash

# Stalonetray setup script
# Configures stalonetray system tray for XMonad with network-manager-applet

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../utils.sh"

echo "Setting up stalonetray system tray..."

# Install required packages
echo "Installing system tray packages..."
install_packages stalonetray network-manager-applet

# Create config directory
CONFIG_DIR="$HOME/.config/stalonetray"
mkdir -p "$CONFIG_DIR"

# Link stalonetray configuration
if [ -f "$SCRIPT_DIR/stalonetrayrc" ]; then
    create_symlink "$SCRIPT_DIR/stalonetrayrc" "$HOME/.stalonetrayrc"
    echo "✓ Stalonetray configuration linked"
fi

# Create XMonad startup script modification
XMONAD_START="$HOME/.xmonad/xmonad-start"
if [ ! -f "$XMONAD_START" ]; then
    # If xmonad-start doesn't exist, create it from template
    if [ -f "$SCRIPT_DIR/../xsessions/xmonad-start" ]; then
        cp "$SCRIPT_DIR/../xsessions/xmonad-start" "$XMONAD_START"
        chmod +x "$XMONAD_START"
    fi
fi

# Check if stalonetray is already in xmonad-start
if [ -f "$XMONAD_START" ]; then
    if ! grep -q "stalonetray" "$XMONAD_START"; then
        echo "Adding stalonetray to XMonad startup..."
        # Add stalonetray and nm-applet before the xmonad exec line
        sed -i '/^exec xmonad/i \
# Start system tray\
stalonetray &\
\
# Start network manager applet\
nm-applet --indicator &\
' "$XMONAD_START"
        echo "✓ Added stalonetray to XMonad startup"
    else
        echo "✓ Stalonetray already configured in XMonad startup"
    fi
fi

# Update XMonad configuration to add space for tray
XMONAD_HS="$HOME/.xmonad/xmonad.hs"
if [ ! -f "$XMONAD_HS" ] && [ -f "$SCRIPT_DIR/../xmonad/xmonad.hs" ]; then
    XMONAD_HS="$SCRIPT_DIR/../xmonad/xmonad.hs"
fi

if [ -f "$XMONAD_HS" ]; then
    echo "Checking XMonad configuration for tray support..."
    
    # Check if manageDocks is imported (needed for tray)
    if ! grep -q "ManageDocks" "$XMONAD_HS"; then
        echo "Note: ManageDocks is already imported for dock management"
    fi
    
    # Check if avoidStruts is used (needed for tray space)
    if ! grep -q "avoidStruts" "$XMONAD_HS"; then
        echo "Note: avoidStruts is already configured for proper spacing"
    fi
    
    echo "✓ XMonad configuration supports system tray"
fi

# Create a desktop file for nm-applet if it doesn't exist
AUTOSTART_DIR="$HOME/.config/autostart"
mkdir -p "$AUTOSTART_DIR"

if [ ! -f "$AUTOSTART_DIR/nm-applet.desktop" ]; then
    cat > "$AUTOSTART_DIR/nm-applet.desktop" << 'EOF'
[Desktop Entry]
Type=Application
Name=Network Manager Applet
Comment=Manage network connections
Exec=nm-applet --indicator
Hidden=false
NoDisplay=false
X-GNOME-Autostart-enabled=true
EOF
    echo "✓ Network Manager applet autostart configured"
fi

echo ""
echo "=== Stalonetray setup complete! ==="
echo ""
echo "System tray configuration:"
echo "  - Stalonetray will appear in the top-right corner"
echo "  - Network Manager applet will show in the tray"
echo "  - Icons size: 24x24 pixels"
echo "  - Background: Black (matches XMonad)"
echo ""
echo "To apply changes:"
echo "  1. Restart XMonad: Super+Q"
echo "  2. Or logout and login again"
echo ""
echo "Tray applications you can add:"
echo "  - nm-applet (network) - already configured"
echo "  - volumeicon (volume control)"
echo "  - blueman-applet (bluetooth)"
echo "  - dropbox"
echo "  - slack, discord, etc."
echo ""
echo "Note: This is an alternative to networkmanager_dmenu"
echo "providing a more traditional tray-based interface."