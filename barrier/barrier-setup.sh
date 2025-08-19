#!/bin/bash

# Input-leap (keyboard/mouse sharing) setup script
# This allows you to share a single keyboard and mouse across multiple computers
# Input-leap is the actively maintained successor to Barrier/Synergy

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOTFILES_DIR="$(dirname "$SCRIPT_DIR")"

# Source utility functions
source "$DOTFILES_DIR/utils.sh"

echo -e "${GREEN}Setting up Input-leap for keyboard/mouse sharing across machines...${NC}"

# Detect OS and package manager
detect_os

if [ "$PKG_MANAGER" = "unknown" ]; then
    echo -e "${RED}Unsupported package manager. Please install Barrier manually.${NC}"
    exit 1
fi

# Install Input-leap
echo -e "${GREEN}Installing Input-leap...${NC}"

# Check if already installed
if command -v input-leap &> /dev/null; then
    echo -e "${YELLOW}Input-leap is already installed${NC}"
else
    case "$PKG_MANAGER" in
        "dnf")
            # Fedora has input-leap in repos
            install_packages input-leap
            ;;
        "apt")
            # Ubuntu - check version and install appropriately
            . /etc/os-release
            if [[ "$VERSION_ID" == "24.04" ]] || [[ "$VERSION_ID" == "24.10" ]]; then
                echo -e "${GREEN}Downloading Input-leap .deb package...${NC}"
                cd /tmp
                wget -q https://github.com/input-leap/input-leap/releases/download/v3.0.2/InputLeap_3.0.2_debian12_amd64.deb
                sudo apt install -y ./InputLeap_3.0.2_debian12_amd64.deb
                rm -f InputLeap_3.0.2_debian12_amd64.deb
            else
                echo -e "${YELLOW}Installing Input-leap via Flatpak (recommended for Ubuntu < 24.04)...${NC}"
                if ! command -v flatpak &> /dev/null; then
                    install_packages flatpak
                fi
                flatpak install -y flathub io.github.input_leap.input-leap
            fi
            ;;
        "pacman")
            # Arch/Manjaro have input-leap in repos
            install_packages input-leap
            ;;
        *)
            echo -e "${YELLOW}Attempting generic package install...${NC}"
            install_packages input-leap
            ;;
    esac
fi

# Create Input-leap config directory
mkdir -p ~/.config/InputLeap

# Create a basic server configuration
cat > ~/.config/InputLeap/InputLeap.conf << 'EOF'
# Input-leap Configuration
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

# Create autostart script for Input-leap
cat > ~/.config/InputLeap/start-input-leap.sh << 'EOF'
#!/bin/bash
# Input-leap autostart script
# Configure these variables for your setup

# Set to "server" or "client"
INPUT_LEAP_MODE="client"

# For client mode, set the server IP/hostname
INPUT_LEAP_SERVER="192.168.1.100"

# Server name (for server mode)
INPUT_LEAP_NAME="$(hostname)"

# Start Input-leap based on mode
if [ "$INPUT_LEAP_MODE" = "server" ]; then
    echo "Starting Input-leap server..."
    input-leaps --no-daemon --name "$INPUT_LEAP_NAME" --config ~/.config/InputLeap/InputLeap.conf &
elif [ "$INPUT_LEAP_MODE" = "client" ]; then
    echo "Starting Input-leap client, connecting to $INPUT_LEAP_SERVER..."
    input-leapc --no-daemon "$INPUT_LEAP_SERVER" &
else
    echo "Invalid INPUT_LEAP_MODE. Set to 'server' or 'client'"
fi
EOF

chmod +x ~/.config/InputLeap/start-input-leap.sh

# Create desktop entry for easy GUI access
mkdir -p ~/.local/share/applications
cat > ~/.local/share/applications/input-leap-config.desktop << EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=Input-leap Configuration
Comment=Configure Input-leap keyboard/mouse sharing
Exec=input-leap
Icon=input-leap
Terminal=false
Categories=System;Network;
EOF

echo -e "\n${GREEN}Input-leap installation complete!${NC}"
echo -e "${YELLOW}Configuration Notes:${NC}"
echo ""
echo "1. GUI Setup (Recommended for first-time setup):"
echo "   Run: input-leap"
echo ""
echo "2. Server Setup:"
echo "   - Run Input-leap GUI and configure as server"
echo "   - Note your server's IP address"
echo "   - Configure screen layout in the GUI"
echo ""
echo "3. Client Setup:"
echo "   - Run Input-leap GUI and configure as client"  
echo "   - Enter the server's IP address"
echo "   - Use the same screen name as configured on server"
echo ""
echo "4. Autostart:"
echo "   Edit ~/.config/InputLeap/start-input-leap.sh with your settings"
echo "   Add to your .xinitrc or session startup:"
echo "   ~/.config/InputLeap/start-input-leap.sh"
echo ""
echo "5. Firewall:"
echo "   Input-leap uses port 24800 by default"
echo "   You may need to open this port on the server machine"
echo ""
# Add important notes based on distribution
if [ "$PKG_MANAGER" = "apt" ]; then
    echo ""
    echo -e "${YELLOW}Important for Ubuntu users:${NC}"
    echo "   - If using Ubuntu 24.04+, Input-leap works best on X11"
    echo "   - For Wayland sessions, switch to 'Ubuntu on Xorg' at login"
    echo "   - For Ubuntu < 24.04, we installed via Flatpak"
fi

echo ""
echo -e "${GREEN}To start configuring now, run: input-leap${NC}"
echo ""
echo "Command-line usage:"
echo "   Server: input-leaps"
echo "   Client: input-leapc <server-ip>"