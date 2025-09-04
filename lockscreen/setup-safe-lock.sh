#!/bin/bash
# Setup a safe automatic lock mechanism using xss-lock
# This approach uses systemd and xss-lock for reliable locking without input deadlock

LOCK_SCRIPT="${HOME}/.config/xmonad/lockscreen/lock-safe.sh"

echo "=== Setting up Safe Automatic Screen Lock ==="
echo ""

# 1. Install xss-lock if needed
if ! command -v xss-lock &> /dev/null; then
    echo "Installing xss-lock..."
    sudo dnf install -y xss-lock
fi

# 2. Create systemd user service for xss-lock
echo "Creating systemd user service..."
mkdir -p ~/.config/systemd/user/

cat > ~/.config/systemd/user/xss-lock.service << EOF
[Unit]
Description=xss-lock - Use external locker as X screen saver
After=graphical-session.target

[Service]
Type=simple
ExecStart=/usr/bin/xss-lock --transfer-sleep-lock -- ${LOCK_SCRIPT}
Restart=always
RestartSec=10

[Install]
WantedBy=graphical-session.target
EOF

# 3. Configure safe DPMS and screensaver settings
echo "Configuring safe screen timeout settings..."
# Disable X screensaver but keep DPMS for power saving
xset s off
xset s noblank
# Set DPMS timeouts (standby, suspend, off) - 10 minutes
xset +dpms
xset dpms 600 600 600

# 4. Create a startup script for X session
echo "Creating X session startup script..."
cat > ~/.config/xmonad/lockscreen/xsession-lock-setup.sh << 'EOF'
#!/bin/bash
# This should be called from .xinitrc or .xprofile

# Kill any existing lock/screensaver programs
pkill -f xautolock
pkill -f xscreensaver
pkill -f light-locker

# Set safe X11 settings
xset s off
xset s noblank
xset +dpms
xset dpms 600 600 600

# Start xss-lock in background
if command -v xss-lock &> /dev/null; then
    xss-lock --transfer-sleep-lock -- ${HOME}/.config/xmonad/lockscreen/lock-safe.sh &
fi
EOF
chmod +x ~/.config/xmonad/lockscreen/xsession-lock-setup.sh

# 5. Enable and start the service
echo "Enabling xss-lock service..."
systemctl --user daemon-reload
systemctl --user enable xss-lock.service
systemctl --user restart xss-lock.service

echo ""
echo "=== Setup Complete ==="
echo ""
echo "Safe automatic locking is now configured:"
echo "  • Screen will lock after 10 minutes of inactivity"
echo "  • Uses xss-lock + i3lock for reliable locking"
echo "  • Prevents the input deadlock issue"
echo ""
echo "Manual lock command:"
echo "  loginctl lock-session"
echo "  OR"
echo "  ${LOCK_SCRIPT}"
echo ""
echo "To check service status:"
echo "  systemctl --user status xss-lock.service"
echo ""
echo "Add this line to your .xinitrc or .xprofile:"
echo "  ~/.config/xmonad/lockscreen/xsession-lock-setup.sh &"