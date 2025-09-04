#!/bin/bash
# Test script for enhanced lock screen functionality
# This tests the new lock screen that should fix the unresponsive input issue

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"

echo "=== Enhanced Lock Screen Test ==="
echo ""
echo "This test will help diagnose and fix the screen lock issues."
echo "The enhanced lock script includes:"
echo "  • Killing conflicting screensaver processes"
echo "  • Proper DPMS management"
echo "  • Better input grab handling"
echo "  • Prevention of screen blanking during lock"
echo ""
echo "Test will begin in 5 seconds..."
echo "Press Ctrl+C to cancel"
echo ""

for i in 5 4 3 2 1; do
    echo -n "$i... "
    sleep 1
done
echo ""

echo "Checking for running screensaver processes..."
ps aux | grep -E "screensaver|locker|xss-lock|light-locker|xautolock" | grep -v grep

echo ""
echo "Testing enhanced lock screen..."
"$SCRIPT_DIR/lock-enhanced.sh"

echo ""
echo "Lock screen test completed!"
echo ""
echo "Did you experience any of these issues?"
echo "  [ ] Screen showed lock warning but keyboard/mouse unresponsive"
echo "  [ ] Screen went completely black"
echo "  [ ] Could not type password"
echo "  [ ] Had to hard reboot"
echo ""
echo "If everything worked correctly, the enhanced lock is ready to use."
echo "To make it permanent, restart your session or run:"
echo "  $HOME/.config/xmonad/lockscreen/auto-lock.sh restart"