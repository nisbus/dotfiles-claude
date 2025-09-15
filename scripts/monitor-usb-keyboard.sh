#!/bin/bash
# Monitor for USB keyboard connections and apply Caps Lock to Ctrl mapping

# Initial mapping
setxkbmap -option ctrl:nocaps
echo "$(date): Initial Caps to Ctrl mapping applied" >> /tmp/caps-monitor.log

# Monitor udev events
udevadm monitor --subsystem-match=usb --subsystem-match=input 2>/dev/null | while read -r line; do
    if echo "$line" | grep -q "add"; then
        # Wait for device to settle
        sleep 1
        
        # Check if a keyboard was added
        if echo "$line" | grep -qi "keyboard\|hid"; then
            setxkbmap -option ctrl:nocaps
            echo "$(date): Reapplied Caps to Ctrl mapping after USB event" >> /tmp/caps-monitor.log
        fi
    fi
done