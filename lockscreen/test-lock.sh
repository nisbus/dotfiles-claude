#!/bin/bash
# Test script for lock screen functionality

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"

echo "=== Lock Screen Test ==="
echo "This will test the lock screen in 5 seconds."
echo "You should see:"
echo "1. A blurred screenshot as background"
echo "2. A password prompt indicator"
echo "3. Time and date display"
echo "4. Be able to type your password to unlock"
echo ""
echo "Press Ctrl+C to cancel, or wait 5 seconds..."

sleep 5

echo "Testing lock screen..."
"$SCRIPT_DIR/lock.sh"

echo "Lock screen test completed!"
echo ""
echo "If you experienced issues:"
echo "1. Screen went black instead of showing lock screen"
echo "2. Could not unlock with password"
echo "3. Mouse/keyboard were unresponsive"
echo ""
echo "Please report the issue."