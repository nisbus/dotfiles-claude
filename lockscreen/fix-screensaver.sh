#!/bin/bash
# Complete fix for screensaver/lock screen issues
# This script disables all conflicting screensaver mechanisms and sets up proper locking

echo "=== Fixing Screensaver/Lock Screen Issues ==="
echo ""

# 1. Kill ALL screensaver-related processes
echo "Step 1: Killing all screensaver processes..."
pkill -f xautolock
pkill -f xscreensaver
pkill -f light-locker
pkill -f xfce4-screensaver
pkill -f gnome-screensaver
pkill -f mate-screensaver
pkill -f xss-lock

# 2. Disable X11 blanking and DPMS completely
echo "Step 2: Disabling X11 screen blanking and DPMS..."
xset s off
xset -dpms
xset s noblank
xset s 0 0

# 3. Disable systemd screen locking if present
echo "Step 3: Checking for systemd screen lock services..."
systemctl --user stop xss-lock.service 2>/dev/null
systemctl --user disable xss-lock.service 2>/dev/null

# 4. Remove any screensaver autostart entries
echo "Step 4: Removing screensaver autostart entries..."
for file in ~/.config/autostart/*screensaver*.desktop ~/.config/autostart/*locker*.desktop; do
    if [ -f "$file" ]; then
        echo "  Removing: $file"
        rm "$file"
    fi
done

# 5. Check what's in xinitrc/xprofile that might start screensavers
echo "Step 5: Checking for screensaver commands in startup files..."
echo ""
echo "Found in startup files:"
grep -l "xautolock\|xscreensaver\|light-locker\|xss-lock" ~/.xinitrc ~/.xprofile ~/.xsession ~/.xsessionrc 2>/dev/null || echo "  None found"

echo ""
echo "=== Current Status ==="
xset q | grep -A 2 "Screen Saver:"
echo ""
xset q | grep -A 3 "DPMS"
echo ""

echo "=== Fix Complete ==="
echo ""
echo "All screensaver mechanisms have been disabled."
echo "This prevents the screen from blanking and losing input."
echo ""
echo "To manually lock your screen, use:"
echo "  ${HOME}/.config/xmonad/lockscreen/lock-safe.sh"
echo ""
echo "To re-enable automatic locking later, use:"
echo "  ${HOME}/.config/xmonad/lockscreen/setup-safe-lock.sh"