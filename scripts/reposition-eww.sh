#!/bin/bash
# Reposition EWW widgets based on current display configuration

# Function to get primary display position
get_primary_display_info() {
    xrandr --query | grep " primary" | head -1 | sed -E 's/.*\s([0-9]+x[0-9]+\+[0-9]+\+[0-9]+).*/\1/'
}

# Parse display info (WIDTHxHEIGHT+X+Y)
DISPLAY_INFO=$(get_primary_display_info)
if [ -z "$DISPLAY_INFO" ]; then
    echo "Could not detect primary display"
    exit 1
fi

# Extract X offset for primary display
X_OFFSET=$(echo "$DISPLAY_INFO" | sed -E 's/.*\+([0-9]+)\+[0-9]+/\1/')

# Close and reopen EWW widgets with adjusted position
eww close-all 2>/dev/null || true
sleep 0.5

# If primary display has an X offset (external monitor on left), adjust widget positions
if [ "$X_OFFSET" -gt 0 ]; then
    # Primary display is not leftmost, update positions temporarily
    export EWW_X_OFFSET="$X_OFFSET"
fi

# Restart widgets
eww open sliders
eww open system_info

echo "EWW widgets repositioned for primary display at offset $X_OFFSET"