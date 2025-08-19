#!/bin/bash

# Input Leap system tray launcher
# Uses yad for system tray integration

# Check if yad is installed
if ! command -v yad &> /dev/null; then
    echo "yad is not installed. Installing..."
    sudo dnf install -y yad || sudo apt-get install -y yad || {
        echo "Failed to install yad. Please install it manually."
        exit 1
    }
fi

# Path to wrapper script
WRAPPER="$HOME/.config/xmonad/scripts/input-leap-wrapper.sh"

# Create menu for yad
export -f handle_menu_action
handle_menu_action() {
    case "$1" in
        "Start Server")
            alacritty --class input-leap-terminal -e "$WRAPPER" server &
            ;;
        "Connect to Server")
            SERVER=$(yad --entry --title="Input Leap" --text="Enter server address:" --width=300)
            if [ -n "$SERVER" ]; then
                alacritty --class input-leap-terminal -e "$WRAPPER" client "$SERVER" &
            fi
            ;;
        "Configure")
            alacritty --class input-leap-terminal -e "$WRAPPER" edit
            ;;
        "Show Config")
            alacritty --class input-leap-terminal -e bash -c "$WRAPPER config; echo; read -p 'Press Enter to close...'"
            ;;
        "Stop")
            pkill -f input-leap
            ;;
        "Quit")
            pkill -f "yad.*input-leap-tray"
            ;;
    esac
}

# Create the system tray icon
yad --notification \
    --image="input-devices" \
    --text="Input Leap" \
    --command="bash -c 'handle_menu_action \"%s\"'" \
    --menu="Start Server|Connect to Server|Configure|Show Config|Stop|Quit" &

# Export the function so it's available to the yad process
export -f handle_menu_action