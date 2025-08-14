#!/bin/bash

# Barrier (keyboard/mouse sharing) setup script
# This allows you to share a single keyboard and mouse across multiple computers

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOTFILES_DIR="$(dirname "$SCRIPT_DIR")"

# Source utility functions
source "$DOTFILES_DIR/utils.sh"

echo -e "${GREEN}Setting up Barrier for keyboard/mouse sharing across machines...${NC}"

# Detect OS and package manager
detect_os

if [ "$PKG_MANAGER" = "unknown" ]; then
    echo -e "${RED}Unsupported package manager. Please install Barrier manually.${NC}"
    exit 1
fi

# Install Barrier
echo -e "${GREEN}Installing Barrier...${NC}"
if command -v barrier &> /dev/null; then
    echo -e "${YELLOW}Barrier is already installed${NC}"
else
    install_packages barrier
fi

# Create Barrier config directory
mkdir -p ~/.local/share/barrier

# Create a basic server configuration
cat > ~/.local/share/barrier/barrier.conf << 'EOF'
# Barrier Configuration
# This is a basic template - modify as needed for your setup

section: screens
    # Define your screens here
    # Example:
    # laptop:
    # desktop:
    # tablet:
end

section: links
    # Define how screens connect
    # Example:
    # laptop:
    #     right = desktop
    # desktop:
    #     left = laptop
end

section: options
    keystroke(f12) = lockCursorToScreen(toggle)
    heartbeat = 5000
    relativeMouseMoves = false
    screenSaverSync = true
    win32KeepForeground = false
    switchCorners = none
    switchCornerSize = 0
    clipboardSharing = true
end
EOF

# Create autostart script for Barrier client
cat > ~/.local/share/barrier/start-barrier.sh << 'EOF'
#!/bin/bash
# Barrier autostart script
# Configure these variables for your setup

# Set to "server" or "client"
BARRIER_MODE="client"

# For client mode, set the server IP/hostname
BARRIER_SERVER="192.168.1.100"

# Server name (for server mode)
BARRIER_NAME="$(hostname)"

# Start Barrier based on mode
if [ "$BARRIER_MODE" = "server" ]; then
    echo "Starting Barrier server..."
    barriers --no-daemon --name "$BARRIER_NAME" --config ~/.local/share/barrier/barrier.conf &
elif [ "$BARRIER_MODE" = "client" ]; then
    echo "Starting Barrier client, connecting to $BARRIER_SERVER..."
    barrierc --no-daemon "$BARRIER_SERVER" &
else
    echo "Invalid BARRIER_MODE. Set to 'server' or 'client'"
fi
EOF

chmod +x ~/.local/share/barrier/start-barrier.sh

# Create desktop entry for easy GUI access
mkdir -p ~/.local/share/applications
cat > ~/.local/share/applications/barrier-config.desktop << EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=Barrier Configuration
Comment=Configure Barrier keyboard/mouse sharing
Exec=barrier
Icon=barrier
Terminal=false
Categories=System;Network;
EOF

echo -e "\n${GREEN}Barrier installation complete!${NC}"
echo -e "${YELLOW}Configuration Notes:${NC}"
echo ""
echo "1. GUI Setup (Recommended for first-time setup):"
echo "   Run: barrier"
echo ""
echo "2. Server Setup:"
echo "   - Run Barrier GUI and configure as server"
echo "   - Note your server's IP address"
echo "   - Configure screen layout in the GUI"
echo ""
echo "3. Client Setup:"
echo "   - Run Barrier GUI and configure as client"  
echo "   - Enter the server's IP address"
echo "   - Use the same screen name as configured on server"
echo ""
echo "4. Autostart:"
echo "   Edit ~/.local/share/barrier/start-barrier.sh with your settings"
echo "   Add to your .xinitrc or session startup:"
echo "   ~/.local/share/barrier/start-barrier.sh"
echo ""
echo "5. Firewall:"
echo "   Barrier uses port 24800 by default"
echo "   You may need to open this port on the server machine"
echo ""
echo -e "${GREEN}To start configuring now, run: barrier${NC}"