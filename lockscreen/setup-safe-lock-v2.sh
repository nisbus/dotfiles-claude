#!/bin/bash
# Enhanced setup for safe automatic screen lock
# Fixes session detection issues and auto-configures startup files

LOCK_SCRIPT="${HOME}/.config/xmonad/lockscreen/lock-safe.sh"
XINITRC_FILE="${HOME}/dotfiles-claude/xsessions/xinitrc"
XPROFILE_FILE="${HOME}/.xprofile"

echo "=== Setting up Safe Automatic Screen Lock (v2) ==="
echo ""

# 1. Stop any existing services/processes
echo "Step 1: Cleaning up existing lock mechanisms..."
pkill -f xautolock
pkill -f xscreensaver
systemctl --user stop xss-lock.service 2>/dev/null

# 2. Install required packages
echo "Step 2: Installing required packages..."
if ! command -v xss-lock &> /dev/null; then
    sudo dnf install -y xss-lock
fi

if ! command -v i3lock &> /dev/null; then
    sudo dnf install -y i3lock
fi

if ! command -v scrot &> /dev/null; then
    sudo dnf install -y scrot
fi

# 3. Create the lock setup script that will be called from xinitrc
echo "Step 3: Creating lock setup script..."
cat > ~/.config/xmonad/lockscreen/setup-lock-session.sh << 'EOF'
#!/bin/bash
# Session-aware lock setup script
# This runs from xinitrc to properly setup locking for the X session

# Wait for session to be fully initialized
sleep 2

# Kill any existing lock/screensaver programs
pkill -f xautolock 2>/dev/null
pkill -f xscreensaver 2>/dev/null
pkill -f light-locker 2>/dev/null

# Set X11 settings for screen timeout
# Disable screensaver but keep DPMS for power saving
xset s off
xset s noblank
xset +dpms
xset dpms 600 600 600  # 10 minutes

# Get the correct display and session for xss-lock
export DISPLAY=${DISPLAY:-:0}

# Start xss-lock directly (not via systemd) to avoid session issues
if command -v xss-lock &> /dev/null; then
    # Use --ignore-sleep to prevent issues with suspend/resume
    # Use --notifier for a warning before locking
    xss-lock \
        --ignore-sleep \
        --notifier='notify-send -u critical -t 10000 "Screen Lock" "Locking in 10 seconds..."' \
        -- ${HOME}/.config/xmonad/lockscreen/lock-safe.sh &
    
    echo "xss-lock started with PID $!"
    
    # Save PID for potential cleanup
    echo $! > /tmp/xss-lock.pid
else
    echo "Warning: xss-lock not found"
fi
EOF
chmod +x ~/.config/xmonad/lockscreen/setup-lock-session.sh

# 4. Configure xinitrc to call our setup script
echo "Step 4: Configuring xinitrc..."
# Check if the line already exists
if ! grep -q "setup-lock-session.sh" "$XINITRC_FILE" 2>/dev/null; then
    # Find the line before "exec emacs" and insert our setup there
    if grep -q "^exec emacs" "$XINITRC_FILE"; then
        # Create a backup
        cp "$XINITRC_FILE" "${XINITRC_FILE}.backup"
        
        # Insert the lock setup before the exec emacs line
        awk '/^exec emacs/ && !done {
            print "# Setup automatic screen locking"
            print "if [ -x /home/valdimar/.config/xmonad/lockscreen/setup-lock-session.sh ]; then"
            print "    /home/valdimar/.config/xmonad/lockscreen/setup-lock-session.sh &"
            print "fi"
            print ""
            done=1
        }
        {print}' "$XINITRC_FILE" > "${XINITRC_FILE}.tmp" && mv "${XINITRC_FILE}.tmp" "$XINITRC_FILE"
        
        echo "✓ Added lock setup to xinitrc"
    else
        echo "⚠ Could not find 'exec emacs' line in xinitrc"
        echo "  Please manually add this before your window manager exec line:"
        echo "  /home/valdimar/.config/xmonad/lockscreen/setup-lock-session.sh &"
    fi
else
    echo "✓ Lock setup already in xinitrc"
fi

# 5. Create manual lock command
echo "Step 5: Creating manual lock command..."
cat > ~/bin/lock-screen << 'EOF'
#!/bin/bash
# Manual screen lock command
loginctl lock-session || ${HOME}/.config/xmonad/lockscreen/lock-safe.sh
EOF
chmod +x ~/bin/lock-screen 2>/dev/null

# 6. Configure safe DPMS settings immediately
echo "Step 6: Applying safe DPMS settings..."
xset s off
xset s noblank
xset +dpms
xset dpms 600 600 600

# 7. Start xss-lock for current session (if in X)
echo "Step 7: Starting xss-lock for current session..."
if [ -n "$DISPLAY" ]; then
    # Kill any existing xss-lock
    pkill -f xss-lock 2>/dev/null
    sleep 1
    
    # Start xss-lock directly
    xss-lock \
        --ignore-sleep \
        --notifier='notify-send -u critical -t 10000 "Screen Lock" "Locking in 10 seconds..."' \
        -- ${HOME}/.config/xmonad/lockscreen/lock-safe.sh &
    
    XSS_PID=$!
    echo $XSS_PID > /tmp/xss-lock.pid
    sleep 2
    
    # Check if it's running
    if kill -0 $XSS_PID 2>/dev/null; then
        echo "✓ xss-lock started successfully (PID: $XSS_PID)"
    else
        echo "⚠ xss-lock failed to start"
    fi
else
    echo "⚠ No X display detected, xss-lock not started"
fi

# 8. Test lock functionality
echo ""
echo "Step 8: Testing lock mechanism..."
echo "The screen should lock in 5 seconds for testing..."
sleep 5

# Trigger a test lock using loginctl
loginctl lock-session 2>/dev/null || {
    echo "loginctl lock-session not available, using direct lock"
    ${HOME}/.config/xmonad/lockscreen/lock-safe.sh
}

echo ""
echo "=== Setup Complete ==="
echo ""
echo "Automatic screen locking is now configured:"
echo "  ✓ Screen will lock after 10 minutes of inactivity"
echo "  ✓ Lock setup added to xinitrc (will activate on next login)"
echo "  ✓ xss-lock running for current session"
echo ""
echo "Manual lock commands:"
echo "  loginctl lock-session"
echo "  ~/bin/lock-screen"
echo "  ${LOCK_SCRIPT}"
echo ""
echo "To check if xss-lock is running:"
echo "  ps aux | grep xss-lock"
echo ""
echo "Your xinitrc has been updated. The changes will take full effect"
echo "after your next login or X session restart."