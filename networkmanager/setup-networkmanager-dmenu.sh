#!/bin/bash

# NetworkManager dmenu setup script
# Installs networkmanager_dmenu for lightweight network management

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../utils.sh"

echo "Setting up networkmanager_dmenu..."

# Check if already installed
if command -v networkmanager_dmenu &> /dev/null; then
    echo "✓ networkmanager_dmenu is already installed"
    exit 0
fi

# Install dependencies
echo "Installing dependencies..."
install_packages git python3 python3-gi python3-gi-cairo gir1.2-nm-1.0 dmenu

# Clone and install networkmanager_dmenu
echo "Installing networkmanager_dmenu from source..."
TEMP_DIR=$(mktemp -d)
cd "$TEMP_DIR"

git clone https://github.com/firecat53/networkmanager-dmenu.git
cd networkmanager-dmenu

# Install the script
sudo cp networkmanager_dmenu /usr/local/bin/
sudo chmod +x /usr/local/bin/networkmanager_dmenu

echo "✓ networkmanager_dmenu installed to /usr/local/bin/"

# Create default config directory
CONFIG_DIR="$HOME/.config/networkmanager-dmenu"
mkdir -p "$CONFIG_DIR"

# Always create/overwrite the config to ensure proper setup
echo "Creating networkmanager_dmenu configuration..."
cat > "$CONFIG_DIR/config.ini" << 'EOF'
[dmenu]
dmenu_command = dmenu -i -l 25 -fn "DejaVu Sans Mono-10"
# dmenu_command = rofi -dmenu -i
# Note that dmenu_command can contain arguments as well
# Vertical list size
l = 25
# Monitor to show on (only rofi)
# m = -1
# Vertical offset (rofi)
# y = 0

[editor]
terminal = alacritty
# GUI nm-connection-editor if available
gui_if_available = true

[nm-applet]
# Emulate nm-applet's floating notifications
notify = false

[dmenu_passphrase]
# Custom dmenu command for password entry (obscured text)
dmenu_command = dmenu -i -fn "DejaVu Sans Mono-10" -nb #222222 -nf #222222
# dmenu_command = rofi -dmenu -i -password

[wifi]
# List saved connections
list_saved = true

[active_connection]
# Show active connection in list (set to false to hide)
show_active = true
EOF
echo "✓ Configuration created at $CONFIG_DIR/config.ini"

# Clean up
cd /
rm -rf "$TEMP_DIR"

echo ""
echo "=== networkmanager_dmenu setup complete! ==="
echo ""
echo "Usage:"
echo "  Press Super+n in XMonad to open network manager menu"
echo "  Or run 'networkmanager_dmenu' from terminal"
echo ""
echo "Configuration file: $CONFIG_DIR/config.ini"
echo ""
echo "Features:"
echo "  - Connect to WiFi networks"
echo "  - Manage VPN connections"
echo "  - Enable/disable networking"
echo "  - Launch nm-connection-editor for advanced settings"