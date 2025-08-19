#!/bin/bash
# Start EWW widgets on the primary display

# Wait for display to stabilize
sleep 1

# Get the primary monitor index for EWW
# EWW uses 0-based indexing
PRIMARY_MONITOR=0

# Check if external display is primary
if xrandr --query | grep -E "^(HDMI|DP|VGA)" | grep -q "primary"; then
    # External is primary, it's usually monitor 0 in EWW
    PRIMARY_MONITOR=0
elif xrandr --query | grep -E "^(eDP|LVDS)" | grep -q "primary"; then
    # Check if there's an external display connected
    if xrandr --query | grep -E "^(HDMI|DP|VGA)" | grep -q " connected"; then
        # Laptop is primary but external exists, laptop is monitor 1
        PRIMARY_MONITOR=1
    else
        # Laptop only, it's monitor 0
        PRIMARY_MONITOR=0
    fi
fi

echo "Starting EWW widgets on monitor $PRIMARY_MONITOR" >> /tmp/eww-start.log

# Close any existing widgets
eww close-all 2>/dev/null || true

# Update EWW config to use the correct monitor
EWW_CONFIG="$HOME/.config/eww/eww.yuck"
if [ -f "$EWW_CONFIG" ]; then
    # Create temporary config with updated monitor
    sed "s/:monitor [0-9]/:monitor $PRIMARY_MONITOR/g" "$EWW_CONFIG" > /tmp/eww.yuck.tmp
    mv /tmp/eww.yuck.tmp "$EWW_CONFIG"
fi

# Open the widgets
eww open sliders
eww open system_info

# Mark widgets as visible
touch /tmp/eww-widgets-visible

echo "EWW widgets started" >> /tmp/eww-start.log