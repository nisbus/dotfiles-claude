#!/bin/bash

# Monitor setup script with autorandr support
# This script configures monitor profiles and sets up automatic detection

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../utils.sh"

echo "Setting up monitor configuration with autorandr..."

# Install required packages
echo "Installing monitor management tools..."
install_packages xrandr arandr autorandr

# Function to detect connected monitors
detect_monitors() {
    xrandr --query | grep " connected" | cut -d" " -f1
}

# Function to get primary monitor (usually the laptop display)
get_laptop_display() {
    # Common laptop display names
    for name in eDP eDP-1 eDP1 LVDS LVDS-1 LVDS1; do
        if xrandr --query | grep "^$name connected" > /dev/null 2>&1; then
            echo "$name"
            return
        fi
    done
    # Fallback: first connected display
    xrandr --query | grep " connected" | head -1 | cut -d" " -f1
}

# Function to get external monitor
get_external_display() {
    local laptop_display="$1"
    xrandr --query | grep " connected" | cut -d" " -f1 | grep -v "^$laptop_display$" | head -1
}

# Create autorandr profiles
setup_autorandr_profiles() {
    echo "Detecting monitors..."
    
    LAPTOP_DISPLAY=$(get_laptop_display)
    EXTERNAL_DISPLAY=$(get_external_display "$LAPTOP_DISPLAY")
    
    echo "Laptop display: $LAPTOP_DISPLAY"
    
    if [ -n "$EXTERNAL_DISPLAY" ]; then
        echo "External display detected: $EXTERNAL_DISPLAY"
        
        # Create dual monitor profile (external as primary)
        echo "Creating dual monitor profile..."
        xrandr --output "$EXTERNAL_DISPLAY" --primary --auto \
               --output "$LAPTOP_DISPLAY" --auto --right-of "$EXTERNAL_DISPLAY"
        
        # Save the dual monitor profile
        autorandr --save dual
        echo "✓ Dual monitor profile saved"
        
        # Create external-only profile
        echo "Creating external-only profile..."
        xrandr --output "$EXTERNAL_DISPLAY" --primary --auto \
               --output "$LAPTOP_DISPLAY" --off
        autorandr --save external
        echo "✓ External-only profile saved"
    else
        echo "No external display detected"
    fi
    
    # Create laptop-only profile
    echo "Creating laptop-only profile..."
    if [ -n "$EXTERNAL_DISPLAY" ]; then
        xrandr --output "$LAPTOP_DISPLAY" --primary --auto \
               --output "$EXTERNAL_DISPLAY" --off
    else
        xrandr --output "$LAPTOP_DISPLAY" --primary --auto
    fi
    autorandr --save laptop
    echo "✓ Laptop-only profile saved"
    
    # Set dual as default if external monitor is connected
    if [ -n "$EXTERNAL_DISPLAY" ]; then
        autorandr --default dual
        echo "✓ Set 'dual' as default profile"
    else
        autorandr --default laptop
        echo "✓ Set 'laptop' as default profile"
    fi
}

# Create autorandr postswitch hook for XMonad
setup_xmonad_hook() {
    echo "Setting up XMonad integration..."
    
    HOOK_DIR="$HOME/.config/autorandr/postswitch"
    mkdir -p "$(dirname "$HOOK_DIR")"
    
    cat > "$HOOK_DIR" << 'EOF'
#!/bin/bash
# Restart XMonad after display change to update workspaces

# Give X server time to settle
sleep 1

# Notify XMonad to restart and redetect screens
if pgrep xmonad > /dev/null; then
    # Send restart signal to XMonad
    xmonad --restart
    
    # Optional: notify the user
    if command -v notify-send > /dev/null; then
        notify-send "Display Configuration" "Monitor profile changed to: $AUTORANDR_CURRENT_PROFILE"
    fi
fi
EOF
    
    chmod +x "$HOOK_DIR"
    echo "✓ XMonad postswitch hook created"
}

# Create systemd user service for autorandr
setup_systemd_service() {
    echo "Setting up systemd service for automatic monitor detection..."
    
    SYSTEMD_USER_DIR="$HOME/.config/systemd/user"
    mkdir -p "$SYSTEMD_USER_DIR"
    
    cat > "$SYSTEMD_USER_DIR/autorandr.service" << 'EOF'
[Unit]
Description=autorandr automatic display configuration
After=graphical-session.target

[Service]
Type=oneshot
ExecStart=/usr/bin/autorandr --change --default laptop
Environment=DISPLAY=:0

[Install]
WantedBy=default.target
EOF
    
    # Enable the service
    systemctl --user daemon-reload
    systemctl --user enable autorandr.service
    echo "✓ Systemd service enabled"
}

# Main setup
echo ""
echo "=== Monitor Setup Configuration ==="
echo ""

# Setup profiles
setup_autorandr_profiles

# Setup XMonad hook
setup_xmonad_hook

# Setup systemd service
setup_systemd_service

echo ""
echo "=== Monitor setup complete! ==="
echo ""
echo "Available profiles:"
autorandr --list
echo ""
echo "Current profile:"
autorandr --current
echo ""
echo "Commands:"
echo "  autorandr --change    # Auto-detect and apply best profile"
echo "  autorandr laptop      # Switch to laptop-only"
echo "  autorandr dual        # Switch to dual monitor"
echo "  autorandr external    # Switch to external-only"
echo ""
echo "Monitor switching will happen automatically when you:"
echo "  - Connect/disconnect external monitors"
echo "  - Log in to your session"
echo "  - XMonad will restart automatically to update workspaces"