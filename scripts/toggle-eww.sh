#!/bin/bash
# Toggle EWW widgets visibility

# Check if EWW daemon is running
if ! pgrep -x "eww" > /dev/null; then
    eww daemon
    sleep 1
fi

# Create a state file to track widget visibility
STATE_FILE="/tmp/eww-widgets-visible"

# List of widgets to toggle
WIDGETS=("sliders" "system_info")

# Check current state
if [ -f "$STATE_FILE" ]; then
    # Widgets are visible, close them
    for widget in "${WIDGETS[@]}"; do
        eww close "$widget" 2>/dev/null || true
    done
    rm -f "$STATE_FILE"
    echo "EWW widgets hidden"
else
    # Widgets are hidden, open them
    for widget in "${WIDGETS[@]}"; do
        eww open "$widget" 2>/dev/null || true
    done
    touch "$STATE_FILE"
    echo "EWW widgets shown"
fi