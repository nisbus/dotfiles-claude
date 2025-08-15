#!/bin/bash
# Toggle EWW widgets visibility

# Check if EWW daemon is running
if ! pgrep -x "eww" > /dev/null; then
    eww daemon
    sleep 1
fi

# List of widgets to toggle
WIDGETS=("sliders" "system_info")

for widget in "${WIDGETS[@]}"; do
    # Check if widget is open
    if eww windows | grep -q "$widget"; then
        eww close "$widget"
    else
        eww open "$widget"
    fi
done