# Stalonetray System Tray for XMonad

This branch demonstrates an alternative approach to network management in XMonad using a traditional system tray with Network Manager applet.

## Overview

Instead of using dmenu-based tools (like networkmanager_dmenu), this approach uses:
- **stalonetray**: A standalone system tray for window managers
- **nm-applet**: The standard Network Manager GUI applet
- **pasystray**: PulseAudio volume control with tray icon
- **blueman-applet**: Bluetooth device management with tray icon

## Features

- Traditional click-based network management interface
- Visual network status indicator in tray
- Volume control with visual feedback
- Bluetooth device management and pairing
- Support for VPN connections
- WiFi signal strength indicator
- Easy network switching with mouse
- Scroll wheel volume adjustment
- Quick connect to Bluetooth devices
- File transfer via Bluetooth
- Works with other tray applications (battery, redshift, etc.)

## Installation

The setup script will:
1. Install stalonetray and network-manager-applet
2. Configure stalonetray to appear in top-right corner
3. Set up nm-applet to start automatically
4. Configure XMonad to reserve space for the tray

Run the setup:
```bash
./stalonetray/setup-stalonetray.sh
```

Or use the main setup script which will prompt you:
```bash
./setup.sh
```

## Configuration

### Stalonetray Configuration
Configuration file: `~/.stalonetrayrc`
- Position: Top-right corner
- Icon size: 24x24 pixels
- Background: Black (matches XMonad)
- Growth direction: Left (new icons appear to the left)

### Volume Control

Pasystray provides PulseAudio control with:
- **Left-click**: Open volume control menu
- **Middle-click**: Mute/unmute
- **Right-click**: Open pavucontrol mixer
- **Scroll wheel**: Adjust volume up/down
- **Icon**: Shows current volume level visually

### Bluetooth Control

Blueman-applet provides Bluetooth management with:
- **Left-click**: Open device menu (connect/disconnect)
- **Right-click**: Open Bluetooth settings
- **Middle-click**: Turn Bluetooth on/off
- **Pairing**: Easy device pairing wizard
- **File transfer**: Send/receive files via Bluetooth

### Adding More Tray Applications

Edit `~/.xmonad/xmonad-start` to add more tray apps:
```bash
# Bluetooth manager
blueman-applet &

# Battery indicator
cbatticon &

# Dropbox
dropbox start -i &
```

## Comparison with networkmanager_dmenu

### Advantages of System Tray:
- More visual feedback (signal strength, connection status)
- Familiar interface for users coming from desktop environments
- Mouse-friendly operation
- Can add multiple system utilities in one place

### Advantages of dmenu approach:
- Keyboard-driven workflow
- Minimal screen real estate usage
- Faster for power users
- Better integration with tiling WM philosophy

## Switching Between Approaches

To switch from tray to dmenu approach:
```bash
git checkout feat-menus
./setup.sh
```

To use the tray approach:
```bash
git checkout feat-trays
./setup.sh
```

## Troubleshooting

### Tray not appearing
- Ensure XMonad was restarted after setup (Super+Q)
- Check if stalonetray is running: `pgrep stalonetray`
- Verify ManageDocks is imported in xmonad.hs

### nm-applet not showing
- Check if NetworkManager service is running: `systemctl status NetworkManager`
- Try starting manually: `nm-applet --indicator &`
- Check for error messages: `nm-applet --indicator`

### Icons too small/large
- Edit `~/.stalonetrayrc` and adjust `icon_size` parameter
- Default is 24, try 16 or 32

## Additional Tray Applications

Popular tray applications that work well with stalonetray:
- `volumeicon`: Volume control
- `blueman-applet`: Bluetooth management
- `udiskie --tray`: USB automounting
- `pasystray`: PulseAudio control
- `cbatticon`: Battery indicator
- `redshift-gtk`: Blue light filter