#!/bin/bash
# Battery icon script for EWW

BATTERY_LEVEL=$(cat /sys/class/power_supply/BAT0/capacity 2>/dev/null || echo 100)
BATTERY_STATUS=$(cat /sys/class/power_supply/BAT0/status 2>/dev/null || echo "Unknown")

if [ "$BATTERY_STATUS" = "Charging" ]; then
    echo ""
elif [ "$BATTERY_LEVEL" -ge 90 ]; then
    echo ""
elif [ "$BATTERY_LEVEL" -ge 70 ]; then
    echo ""
elif [ "$BATTERY_LEVEL" -ge 50 ]; then
    echo ""
elif [ "$BATTERY_LEVEL" -ge 30 ]; then
    echo ""
else
    echo ""
fi