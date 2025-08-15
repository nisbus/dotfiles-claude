#!/bin/bash
# Toggle EWW cheatsheet window

# Check if EWW daemon is running
if ! pgrep -x "eww" > /dev/null; then
    eww daemon
    sleep 1
fi

# Check if cheatsheet is open
if eww active-windows | grep -q "cheatsheet"; then
    eww close cheatsheet
else
    eww open cheatsheet
fi