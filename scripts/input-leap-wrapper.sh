#!/bin/bash

# Input Leap wrapper script for XMonad
# Handles system tray integration and configuration

# Kill any existing input-leap instances
pkill -f input-leap

# Check if configuration exists
CONFIG_DIR="$HOME/.config/InputLeap"
CONFIG_FILE="$CONFIG_DIR/InputLeap.conf"

if [ ! -d "$CONFIG_DIR" ]; then
    mkdir -p "$CONFIG_DIR"
fi

# Create a basic configuration if it doesn't exist
if [ ! -f "$CONFIG_FILE" ]; then
    cat > "$CONFIG_FILE" << 'EOF'
section: screens
    localhost:
        halfDuplexCapsLock = false
        halfDuplexNumLock = false
        halfDuplexScrollLock = false
        xtestIsXineramaUnaware = false
        preserveFocus = false
        switchCorners = none
        switchCornerSize = 0
end

section: aliases
end

section: links
    localhost:
        right = localhost
        left = localhost
end

section: options
    relativeMouseMoves = false
    screenSaverSync = true
    win32KeepForeground = false
    clipboardSharing = true
    switchCorners = none
    switchCornerSize = 0
end
EOF
    echo "Created default InputLeap configuration at $CONFIG_FILE"
fi

# Function to run input-leap in client mode
run_client() {
    echo "Starting Input Leap in client mode..."
    echo "Server address: $1"
    input-leapc --no-tray --name "$(hostname)" "$1"
}

# Function to run input-leap in server mode
run_server() {
    echo "Starting Input Leap in server mode..."
    # Run server without GUI
    input-leaps -c "$CONFIG_FILE" --no-daemon --name "$(hostname)"
}

# Function to show configuration in terminal
show_config() {
    echo "=== Input Leap Configuration ==="
    echo
    if [ -f "$CONFIG_FILE" ]; then
        cat "$CONFIG_FILE"
    else
        echo "No configuration file found."
    fi
    echo
    echo "=== Configuration Help ==="
    echo "1. Edit configuration: $CONFIG_FILE"
    echo "2. Add screens in the 'screens' section"
    echo "3. Define screen layout in the 'links' section"
    echo "4. Example for two computers:"
    echo "   section: screens"
    echo "       computer1:"
    echo "       computer2:"
    echo "   end"
    echo "   section: links"
    echo "       computer1:"
    echo "           right = computer2"
    echo "       computer2:"
    echo "           left = computer1"
    echo "   end"
}

# Function to edit configuration
edit_config() {
    if [ -n "$EDITOR" ]; then
        $EDITOR "$CONFIG_FILE"
    else
        nano "$CONFIG_FILE"
    fi
}

# Parse command line arguments
case "$1" in
    server)
        run_server
        ;;
    client)
        if [ -z "$2" ]; then
            echo "Error: Please provide server address"
            echo "Usage: $0 client <server-address>"
            exit 1
        fi
        run_client "$2"
        ;;
    config)
        show_config
        ;;
    edit)
        edit_config
        ;;
    *)
        echo "Input Leap Wrapper for XMonad"
        echo "=============================="
        echo
        echo "Usage:"
        echo "  $0 server              - Start as server (share this computer's keyboard/mouse)"
        echo "  $0 client <address>    - Start as client (use another computer's keyboard/mouse)"
        echo "  $0 config              - Show current configuration"
        echo "  $0 edit                - Edit configuration file"
        echo
        echo "Examples:"
        echo "  $0 server"
        echo "  $0 client 192.168.1.100"
        echo
        echo "Note: The GUI configuration may crash in XMonad. Use this wrapper instead."
        ;;
esac