#!/bin/bash
# Check the status of the screen lock setup

echo "=== Screen Lock Status Check ==="
echo ""

# 1. Check if xss-lock is running
echo "1. xss-lock process:"
if pgrep -f xss-lock > /dev/null; then
    echo "   ✓ xss-lock is running"
    ps aux | grep -E "[x]ss-lock" | awk '{print "     PID:", $2, "CMD:", $11, $12, $13, $14}'
else
    echo "   ✗ xss-lock is NOT running"
fi
echo ""

# 2. Check systemd service (if configured)
echo "2. Systemd service status:"
if systemctl --user list-unit-files | grep -q xss-lock.service; then
    STATUS=$(systemctl --user is-active xss-lock.service 2>/dev/null)
    if [ "$STATUS" = "active" ]; then
        echo "   ✓ xss-lock.service is active"
    else
        echo "   ✗ xss-lock.service is $STATUS"
    fi
    
    # Check for the session error
    if journalctl --user -u xss-lock.service -n 5 --no-pager 2>/dev/null | grep -q "NoSessionForPID"; then
        echo "   ⚠ Warning: Session detection error (use xinitrc method instead)"
    fi
else
    echo "   - No systemd service configured (OK - using xinitrc method)"
fi
echo ""

# 3. Check X11 settings
echo "3. X11 Display Power Management:"
DPMS_STATUS=$(xset q | grep "DPMS is" | awk '{print $3}')
if [ "$DPMS_STATUS" = "Enabled" ]; then
    echo "   ✓ DPMS is Enabled"
    xset q | grep "Standby:" | sed 's/^/     /'
else
    echo "   ✗ DPMS is Disabled"
fi

echo ""
echo "4. X11 Screen Saver:"
SCREENSAVER_TIMEOUT=$(xset q | grep "timeout:" | awk '{print $2}')
if [ "$SCREENSAVER_TIMEOUT" = "0" ]; then
    echo "   ✓ X11 screensaver disabled (correct)"
else
    echo "   ⚠ X11 screensaver timeout: $SCREENSAVER_TIMEOUT seconds"
fi
echo ""

# 4. Check for conflicting screensavers
echo "5. Conflicting screensaver processes:"
CONFLICTS=0
for PROCESS in xautolock xscreensaver light-locker gnome-screensaver; do
    if pgrep -x "$PROCESS" > /dev/null; then
        echo "   ⚠ $PROCESS is running (should be disabled)"
        CONFLICTS=$((CONFLICTS + 1))
    fi
done
if [ $CONFLICTS -eq 0 ]; then
    echo "   ✓ No conflicting screensavers found"
fi
echo ""

# 5. Check lock script
echo "6. Lock script availability:"
LOCK_SCRIPT="$HOME/.config/xmonad/lockscreen/lock-safe.sh"
if [ -x "$LOCK_SCRIPT" ]; then
    echo "   ✓ Lock script exists and is executable"
    echo "     $LOCK_SCRIPT"
else
    echo "   ✗ Lock script not found or not executable"
fi
echo ""

# 6. Check xinitrc configuration
echo "7. xinitrc configuration:"
XINITRC="$HOME/dotfiles-claude/xsessions/xinitrc"
if [ -f "$XINITRC" ]; then
    if grep -q "setup-lock-session.sh" "$XINITRC"; then
        echo "   ✓ Lock setup found in xinitrc"
    else
        echo "   ✗ Lock setup NOT in xinitrc"
        echo "     Run: ./lockscreen/setup-safe-lock-v2.sh"
    fi
else
    echo "   ⚠ xinitrc not found at expected location"
fi
echo ""

# 7. Check if we can lock
echo "8. Lock capability test:"
if command -v loginctl &> /dev/null; then
    SESSION_ID=$(loginctl list-sessions --no-legend | head -1 | awk '{print $1}')
    if [ -n "$SESSION_ID" ]; then
        echo "   ✓ loginctl available (session: $SESSION_ID)"
        echo "     Can lock with: loginctl lock-session"
    else
        echo "   ⚠ No active session found"
    fi
else
    echo "   ⚠ loginctl not available"
fi
echo ""

# Summary
echo "=== Summary ==="
if pgrep -f xss-lock > /dev/null && [ "$DPMS_STATUS" = "Enabled" ] && [ $CONFLICTS -eq 0 ]; then
    echo "✅ Screen lock is properly configured and running"
    echo ""
    echo "Lock commands you can use:"
    echo "  • loginctl lock-session"
    echo "  • $LOCK_SCRIPT"
else
    echo "⚠️  Screen lock needs configuration"
    echo ""
    echo "To fix, run:"
    echo "  ./lockscreen/setup-safe-lock-v2.sh"
fi