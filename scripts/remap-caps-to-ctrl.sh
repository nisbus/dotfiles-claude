#!/bin/bash
# Script to remap Caps Lock to Ctrl
# Called by udev when USB keyboard is connected

# Wait a moment for the device to fully initialize
sleep 1

# Get the current display
export DISPLAY=:0

# Find the active user (assuming single user system)
USER_NAME=$(who | grep ':0' | head -n1 | awk '{print $1}')
if [ -z "$USER_NAME" ]; then
    # Fallback to getting user from the home directory
    USER_NAME=$(ls -ld /home/valdimar | awk '{print $3}')
fi

# Set the XAUTHORITY for X11 access
export XAUTHORITY="/home/$USER_NAME/.Xauthority"

# Apply the keyboard mapping
# Using ctrl:nocaps to make Caps Lock act as Ctrl
su - "$USER_NAME" -c "DISPLAY=:0 setxkbmap -option ctrl:nocaps" 2>/dev/null

# Log the action for debugging
echo "$(date): Remapped Caps Lock to Ctrl for user $USER_NAME" >> /tmp/caps-to-ctrl.log