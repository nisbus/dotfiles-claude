#!/bin/bash
# Setup GNOME screensaver with security-compliant settings
# Compatible with both Fedora and Ubuntu

echo "=== Setting up GNOME Screensaver ==="
echo "Compatible with Fedora and Ubuntu"
echo ""

# 1. Stop any conflicting lock mechanisms
echo "Step 1: Stopping conflicting lock services..."
pkill -x xautolock 2>/dev/null
pkill -x xscreensaver 2>/dev/null
pkill -x light-locker 2>/dev/null

# 2. Enable GNOME screensaver with proper settings
echo "Step 2: Configuring GNOME screensaver..."

# Set idle delay to 10 minutes (600 seconds) for security compliance
gsettings set org.gnome.desktop.session idle-delay 600

# Enable screensaver on idle
gsettings set org.gnome.desktop.screensaver idle-activation-enabled true

# Enable lock screen
gsettings set org.gnome.desktop.screensaver lock-enabled true

# Lock immediately when screensaver activates (0 second delay)
gsettings set org.gnome.desktop.screensaver lock-delay 0

# Show user switch option on lock screen
gsettings set org.gnome.desktop.screensaver user-switch-enabled true

# Show notifications on lock screen (optional, can be disabled for security)
gsettings set org.gnome.desktop.screensaver show-notifications false

# 3. Configure screen lock settings
echo "Step 3: Configuring screen lock behavior..."

# Ensure lock screen shows on suspend
gsettings set org.gnome.desktop.screensaver ubuntu-lock-on-suspend true 2>/dev/null || true

# Set lock screen background (optional)
# gsettings set org.gnome.desktop.screensaver picture-uri 'file:///usr/share/backgrounds/gnome/adwaita-l.jpg'

# 4. Configure notifications for lock events
echo "Step 4: Setting up lock notifications..."

# Enable notifications in general
gsettings set org.gnome.desktop.notifications show-in-lock-screen false

# 5. Set power management to not interfere
echo "Step 5: Configuring power management..."

# Ensure screen doesn't blank too early (set to 15 minutes)
gsettings set org.gnome.desktop.session idle-delay 600
gsettings set org.gnome.settings-daemon.plugins.power idle-dim false 2>/dev/null || true

# For laptops: what to do on lid close
gsettings set org.gnome.settings-daemon.plugins.power lid-close-ac-action 'lock' 2>/dev/null || true
gsettings set org.gnome.settings-daemon.plugins.power lid-close-battery-action 'lock' 2>/dev/null || true

echo ""
echo "=== Current Configuration ==="
echo "Idle timeout: $(gsettings get org.gnome.desktop.session idle-delay) seconds"
echo "Screensaver enabled: $(gsettings get org.gnome.desktop.screensaver idle-activation-enabled)"
echo "Lock enabled: $(gsettings get org.gnome.desktop.screensaver lock-enabled)"
echo "Lock delay: $(gsettings get org.gnome.desktop.screensaver lock-delay) seconds"

echo ""
echo "=== Testing Commands ==="
echo "Test lock immediately:"
echo "  Fedora/GNOME 40+: gdbus call --session --dest org.gnome.ScreenSaver --object-path /org/gnome/ScreenSaver --method org.gnome.ScreenSaver.Lock"
echo "  Ubuntu/older GNOME: gnome-screensaver-command -l"
echo ""
echo "Check if screen is locked:"
echo "  gdbus call --session --dest org.gnome.ScreenSaver --object-path /org/gnome/ScreenSaver --method org.gnome.ScreenSaver.GetActive"

echo ""
echo "=== Setup Complete ==="
echo "✓ Screen will lock after 10 minutes of inactivity"
echo "✓ Lock screen will activate immediately when triggered"
echo "✓ Compatible with both Fedora and Ubuntu"
echo ""
echo "Note: You may need to log out and back in for all changes to take effect."