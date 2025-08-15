#!/bin/bash
# Toggle EWW power menu window

# Check if EWW daemon is running
if ! pgrep -x "eww" > /dev/null; then
    eww daemon
    sleep 1
fi

# Check if power_menu is open
if eww active-windows | grep -q "power_menu"; then
    eww close power_menu
else
    eww open power_menu
fi