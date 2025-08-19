#!/bin/bash

# Auto-detect network interfaces and configure xmobar

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEMPLATE="$SCRIPT_DIR/xmobarrc.template"
OUTPUT="$SCRIPT_DIR/xmobarrc"

# Detect network interfaces
WIFI_IFACE=$(ip link show | grep -E "^[0-9]+: wlp" | cut -d: -f2 | tr -d ' ' | head -1)
ETH_IFACE=$(ip link show | grep -E "^[0-9]+: (enp|eth)" | cut -d: -f2 | tr -d ' ' | head -1)

# Build network monitors configuration
NETWORK_MONITORS=""
NETWORK_TEMPLATE=""

if [ -n "$ETH_IFACE" ]; then
    NETWORK_MONITORS=", Run Network \"$ETH_IFACE\" [\"-t\",\"Eth: <rx>kb/<tx>kb\",\"-L\",\"0\",\"-H\",\"32\",\"--normal\",\"green\",\"--high\",\"red\"] 10"
    NETWORK_TEMPLATE="%$ETH_IFACE%"
fi

if [ -n "$WIFI_IFACE" ]; then
    if [ -n "$NETWORK_MONITORS" ]; then
        NETWORK_MONITORS="$NETWORK_MONITORS
                    , Run Network \"$WIFI_IFACE\" [\"-t\",\"WiFi: <rx>kb/<tx>kb\",\"-L\",\"0\",\"-H\",\"32\",\"--normal\",\"green\",\"--high\",\"red\"] 10"
        NETWORK_TEMPLATE="$NETWORK_TEMPLATE | %$WIFI_IFACE%"
    else
        NETWORK_MONITORS=", Run Network \"$WIFI_IFACE\" [\"-t\",\"WiFi: <rx>kb/<tx>kb\",\"-L\",\"0\",\"-H\",\"32\",\"--normal\",\"green\",\"--high\",\"red\"] 10"
        NETWORK_TEMPLATE="%$WIFI_IFACE%"
    fi
fi

# If no network interfaces found, add a placeholder
if [ -z "$NETWORK_MONITORS" ]; then
    NETWORK_MONITORS="-- No network interfaces detected"
    NETWORK_TEMPLATE="No network"
fi

# Create the xmobar config from template
cp "$TEMPLATE" "$OUTPUT"

# Replace placeholders
sed -i "s|__NETWORK_MONITORS__|$NETWORK_MONITORS|g" "$OUTPUT"
sed -i "s|__NETWORK_TEMPLATE__|$NETWORK_TEMPLATE|g" "$OUTPUT"

echo "XMobar configured with:"
[ -n "$ETH_IFACE" ] && echo "  - Ethernet: $ETH_IFACE"
[ -n "$WIFI_IFACE" ] && echo "  - WiFi: $WIFI_IFACE"
[ -z "$ETH_IFACE" ] && [ -z "$WIFI_IFACE" ] && echo "  - No network interfaces detected"