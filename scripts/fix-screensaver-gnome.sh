#!/bin/bash
# Fix screensaver/lock screen for GNOME + XMonad environment
# This ensures proper idle detection and screen locking works

echo "=== Fixing Screensaver/Lock Screen for GNOME + XMonad ==="
echo ""

# 1. Disable GNOME's built-in screensaver completely
echo "Step 1: Disabling GNOME screensaver and idle detection..."
gsettings set org.gnome.desktop.session idle-delay 0
gsettings set org.gnome.desktop.screensaver idle-activation-enabled false
gsettings set org.gnome.desktop.screensaver lock-enabled false
gsettings set org.gnome.desktop.screensaver lock-delay 0

# 2. Disable any GNOME screen lock services
echo "Step 2: Disabling GNOME screen lock services..."
systemctl --user stop org.gnome.ScreenSaver.service 2>/dev/null
systemctl --user mask org.gnome.ScreenSaver.service 2>/dev/null

# 3. Kill any conflicting screensaver processes
echo "Step 3: Stopping conflicting screensaver processes..."
pkill -f gnome-screensaver
pkill -f xscreensaver
pkill -f light-locker
pkill -f xfce4-screensaver

# 4. Ensure X11 DPMS is properly configured
echo "Step 4: Configuring X11 display power management..."
xset s off
xset -dpms
xset s noblank

# 5. Restart xautolock with proper configuration
echo "Step 5: Restarting xautolock..."
pkill -x xautolock
sleep 1

# Start xautolock with the auto-lock script
if [ -x "$HOME/.config/xmonad/lockscreen/auto-lock.sh" ]; then
    "$HOME/.config/xmonad/lockscreen/auto-lock.sh" start
else
    echo "Warning: auto-lock.sh not found, starting xautolock manually..."
    xautolock -time 10 -locker "$HOME/.config/xmonad/lockscreen/lock.sh" -secure &
fi

echo ""
echo "=== Current Status ==="
echo "GNOME Settings:"
gsettings get org.gnome.desktop.session idle-delay
gsettings get org.gnome.desktop.screensaver idle-activation-enabled
gsettings get org.gnome.desktop.screensaver lock-enabled

echo ""
echo "X11 Settings:"
xset q | grep -A 2 "Screen Saver:"
echo ""
xset q | grep -A 3 "DPMS"

echo ""
echo "Running processes:"
ps aux | grep -E "(xautolock|screensaver)" | grep -v grep

echo ""
echo "=== Fix Complete ==="
echo "The screensaver should now work properly after 10 minutes of idle time."
echo "You can test it immediately with: xautolock -locknow"
echo "Or adjust the timeout in: ~/.config/xmonad/auto-lock.conf"