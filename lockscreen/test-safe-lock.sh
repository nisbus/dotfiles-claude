#!/bin/bash
# Test the safe lock implementation

echo "=== Testing Safe Lock Screen ==="
echo ""
echo "This will test the new safe lock implementation in 5 seconds."
echo "The safe lock:"
echo "  • Prevents screen blanking during lock"
echo "  • Uses simple, reliable i3lock configuration"
echo "  • Avoids input deadlock issues"
echo ""
echo "Press Ctrl+C to cancel..."
echo ""

for i in 5 4 3 2 1; do
    echo -n "$i... "
    sleep 1
done
echo ""

echo "Testing safe lock..."
/home/valdimar/.config/xmonad/lockscreen/lock-safe.sh

echo ""
echo "Test complete!"
echo ""
echo "If the lock worked correctly without freezing input:"
echo "  Run: ./lockscreen/setup-safe-lock.sh"
echo "  To enable automatic locking with the safe mechanism."