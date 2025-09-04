#!/bin/bash
# DPMS control script for screen locking
# This script manages Display Power Management to prevent screen blanking issues

# Function to disable DPMS and screensaver
disable_dpms() {
    # Check if xset is available
    if command -v xset &> /dev/null; then
        # Save current DPMS state
        xset q | grep "DPMS is Enabled" > /tmp/dpms-state.tmp 2>/dev/null
        
        # Disable DPMS
        xset s off
        xset -dpms
        xset s noblank
        
        # Also disable screen blanking timeout
        xset s 0 0
        
        echo "DPMS disabled for screen lock"
    fi
    
    # If using xscreensaver, deactivate it
    if command -v xscreensaver-command &> /dev/null; then
        xscreensaver-command -deactivate >/dev/null 2>&1
    fi
}

# Function to restore DPMS settings
restore_dpms() {
    if command -v xset &> /dev/null; then
        # Restore default settings
        xset s on
        xset +dpms
        
        # Set reasonable timeouts (in seconds)
        # Standby after 10 minutes, suspend after 15, off after 20
        xset dpms 600 900 1200
        
        echo "DPMS restored"
    fi
}

# Main logic
case "$1" in
    disable)
        disable_dpms
        ;;
    restore)
        restore_dpms
        ;;
    *)
        echo "Usage: $0 {disable|restore}"
        echo "  disable - Turn off DPMS and screensaver"
        echo "  restore - Restore DPMS settings"
        ;;
esac