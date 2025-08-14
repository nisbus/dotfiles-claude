#!/bin/bash

# Screenshot setup script for XMonad
# Configures flameshot for screenshot functionality

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../utils.sh"

echo "Setting up screenshot functionality..."

# Install flameshot
echo "Installing flameshot..."
install_packages flameshot

# Create screenshots directory
SCREENSHOT_DIR="$HOME/Pictures/Screenshots"
if [ ! -d "$SCREENSHOT_DIR" ]; then
    mkdir -p "$SCREENSHOT_DIR"
    echo "✓ Created screenshot directory: $SCREENSHOT_DIR"
fi

# Configure flameshot
echo "Configuring flameshot..."
mkdir -p "$HOME/.config/flameshot"

# Create minimal flameshot configuration
# Let flameshot use mostly defaults to avoid configuration errors
cat > "$HOME/.config/flameshot/flameshot.ini" << EOF
[General]
disabledTrayIcon=true
savePath=$HOME/Pictures/Screenshots
savePathFixed=false
showDesktopNotification=true
startupLaunch=false
EOF

echo "✓ Flameshot configured with minimal settings"

# Set up autostart for flameshot daemon (optional)
AUTOSTART_DIR="$HOME/.config/autostart"
mkdir -p "$AUTOSTART_DIR"

cat > "$AUTOSTART_DIR/flameshot.desktop" << 'EOF'
[Desktop Entry]
Type=Application
Name=Flameshot
Comment=Screenshot tool
Exec=flameshot
Hidden=false
NoDisplay=false
X-GNOME-Autostart-enabled=true
EOF

echo "✓ Flameshot autostart configured"

echo ""
echo "=== Screenshot setup complete! ==="
echo ""
echo "Screenshot keybindings in XMonad:"
echo "  PrtScr        : Open flameshot GUI (interactive mode)"
echo "  Shift+PrtScr  : Full screen to clipboard"
echo "  Ctrl+PrtScr   : Current screen to clipboard"
echo ""
echo "Flameshot features:"
echo "  - Draw annotations (arrows, text, shapes)"
echo "  - Blur sensitive information"
echo "  - Copy to clipboard or save to file"
echo "  - Upload to online services"
echo "  - Pin screenshots to screen"
echo ""
echo "Screenshots will be saved to: $SCREENSHOT_DIR"
echo ""
echo "To apply changes:"
echo "  1. Restart XMonad: Super+Q"
echo "  2. Or reload configuration: Super+Shift+Q"
echo ""
echo "Flameshot GUI controls:"
echo "  - Click and drag to select area"
echo "  - Use toolbar for annotation tools"
echo "  - Ctrl+C to copy to clipboard"
echo "  - Ctrl+S to save to file"
echo "  - Right-click or click outside selection to cancel"
echo "  - Note: Esc may not work in some window managers"