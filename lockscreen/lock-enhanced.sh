#!/bin/bash
# Enhanced lock screen script with proper input handling and screensaver coordination
# Solves the issue of unresponsive keyboard/mouse after screensaver timeout

TEMP_IMAGE="/tmp/xmonad-lock.jpg"
SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
LOCKFILE="/tmp/xmonad-lock.pid"

# Colors for i3lock (if using i3lock-color)
BG="1a1a1a"
FG="d0d0d0"
RING="2a2a2a"
WRONG="bf616a"
DATE="d0d0d0"
VERIFY="a3be8c"

# Function to clean up on exit
cleanup() {
    rm -f "$LOCKFILE"
    rm -f "$TEMP_IMAGE"
}
trap cleanup EXIT

# Function to kill any existing screensavers
kill_screensavers() {
    # Kill xscreensaver if running
    if pgrep -x "xscreensaver" > /dev/null; then
        xscreensaver-command -exit 2>/dev/null || killall xscreensaver 2>/dev/null
    fi
    
    # Kill any existing i3lock instances
    if pgrep -x "i3lock" > /dev/null; then
        killall i3lock 2>/dev/null
        sleep 0.5
    fi
    
    # Kill light-locker if running
    if pgrep -x "light-locker" > /dev/null; then
        killall light-locker 2>/dev/null
    fi
    
    # Kill xfce4-screensaver if running
    if pgrep -x "xfce4-screensaver" > /dev/null; then
        xfce4-screensaver-command --exit 2>/dev/null || killall xfce4-screensaver 2>/dev/null
    fi
}

# Function to disable all power management and screensavers
disable_all_blanking() {
    # Disable DPMS
    if command -v xset &> /dev/null; then
        xset s off
        xset -dpms
        xset s noblank
        xset s 0 0
        
        # Force display on
        xset dpms force on
    fi
    
    # Disable xscreensaver if it's running
    if command -v xscreensaver-command &> /dev/null; then
        xscreensaver-command -deactivate >/dev/null 2>&1
    fi
    
    # Disable GNOME screensaver
    if command -v gsettings &> /dev/null; then
        gsettings set org.gnome.desktop.session idle-delay 0 2>/dev/null
        gsettings set org.gnome.desktop.screensaver lock-enabled false 2>/dev/null
    fi
    
    # Disable KDE screensaver
    if command -v qdbus &> /dev/null; then
        qdbus org.freedesktop.ScreenSaver /ScreenSaver org.freedesktop.ScreenSaver.Inhibit "lock-screen" "Screen locked" 2>/dev/null
    fi
    
    # Prevent systemd from suspending
    if command -v systemctl &> /dev/null; then
        systemd-inhibit --what=handle-lid-switch:sleep:idle --who="lock-screen" --why="Screen locked" --mode=block sleep 1 &
    fi
}

# Function to restore power management
restore_power_management() {
    if command -v xset &> /dev/null; then
        xset s on
        xset +dpms
        xset dpms 600 900 1200
    fi
    
    # Re-enable GNOME screensaver settings
    if command -v gsettings &> /dev/null; then
        gsettings set org.gnome.desktop.session idle-delay 900 2>/dev/null
        gsettings set org.gnome.desktop.screensaver lock-enabled true 2>/dev/null
    fi
}

# Function to ensure dependencies are installed
check_dependencies() {
    local missing_deps=""
    
    for cmd in maim i3lock xset; do
        if ! command -v "$cmd" &> /dev/null; then
            missing_deps="$missing_deps $cmd"
        fi
    done
    
    if [ -n "$missing_deps" ]; then
        echo "Installing missing dependencies:$missing_deps"
        sudo dnf install -y maim i3lock xset imagemagick
    fi
}

# Main lock function
lock_screen() {
    # Check if already locked
    if [ -f "$LOCKFILE" ]; then
        if kill -0 $(cat "$LOCKFILE") 2>/dev/null; then
            echo "Lock screen already active"
            exit 1
        else
            rm -f "$LOCKFILE"
        fi
    fi
    
    # Check dependencies
    check_dependencies
    
    # Kill any existing screensavers first
    kill_screensavers
    
    # Disable all blanking mechanisms
    disable_all_blanking
    
    # Take screenshot and blur it
    echo "Taking screenshot..."
    maim -d 0.1 "$TEMP_IMAGE"
    
    # Apply blur effect
    if command -v convert &> /dev/null; then
        convert "$TEMP_IMAGE" -blur 0x8 "$TEMP_IMAGE"
    elif command -v magick &> /dev/null; then
        magick "$TEMP_IMAGE" -blur 0x8 "$TEMP_IMAGE"
    fi
    
    # Save PID
    echo $$ > "$LOCKFILE"
    
    # Lock with i3lock
    # Using forking mode without -n to prevent blocking
    # The -e flag shows PAM errors, -t for tiling
    echo "Locking screen..."
    
    # Try i3lock-color first (if available) with more options
    if command -v i3lock-color &> /dev/null; then
        i3lock-color \
            -i "$TEMP_IMAGE" \
            -e \
            -t \
            --indicator \
            --radius=120 \
            --ring-width=10 \
            --inside-color="${BG}88" \
            --ring-color="${RING}88" \
            --insidever-color="${VERIFY}88" \
            --ringver-color="${VERIFY}88" \
            --insidewrong-color="${WRONG}88" \
            --ringwrong-color="${WRONG}88" \
            --line-color=00000000 \
            --keyhl-color="${FG}88" \
            --bshl-color="${WRONG}88" \
            --separator-color=00000000 \
            --verif-color="${FG}ff" \
            --wrong-color="${WRONG}ff" \
            --modif-color="${FG}ff" \
            --date-color="${DATE}ff" \
            --time-color="${DATE}ff" \
            --time-str="%H:%M" \
            --date-str="%A, %B %d" \
            --verif-text="Verifying..." \
            --wrong-text="Wrong!" \
            --noinput-text="" \
            --lock-text="Locking..." \
            --lockfailed-text="Lock Failed!" \
            --pass-media-keys \
            --pass-screen-keys \
            --pass-volume-keys \
            -n
    else
        # Fallback to standard i3lock
        # Use -n for non-forking mode to ensure we maintain control
        i3lock -n -i "$TEMP_IMAGE" -e -t
    fi
    
    # Restore power management after unlock
    restore_power_management
    
    # Clean up
    cleanup
}

# Handle signals to ensure cleanup
trap 'restore_power_management; cleanup; exit' INT TERM

# Main execution
lock_screen