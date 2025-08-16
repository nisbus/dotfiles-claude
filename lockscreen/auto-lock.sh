#!/bin/bash
# Auto-lock configuration for XMonad using xautolock
# This script manages idle-based screen locking with configurable timeout

# Configuration file path
CONFIG_FILE="$HOME/.config/xmonad/auto-lock.conf"
LOCK_SCRIPT="$HOME/.config/xmonad/lockscreen/lock.sh"

# Default settings (in minutes)
DEFAULT_IDLE_TIME=10
DEFAULT_NOTIFY_TIME=1

# Load configuration or create default
load_config() {
    if [ -f "$CONFIG_FILE" ]; then
        source "$CONFIG_FILE"
    else
        # Create default configuration
        cat > "$CONFIG_FILE" << EOF
# Auto-lock configuration
# Time in minutes before locking (company policy compliant)
IDLE_TIME=${DEFAULT_IDLE_TIME}

# Time in minutes before lock to show notification (set to 0 to disable)
NOTIFY_TIME=${DEFAULT_NOTIFY_TIME}

# Lock method: blur or wallpaper
LOCK_METHOD="blur"

# Enable corner detection (move mouse to corners to prevent/trigger lock)
# Corners: topleft, topright, bottomleft, bottomright
# Use + to trigger lock, - to prevent lock
CORNERS_ENABLED=true
CORNER_DELAY=5  # seconds
CORNER_REDELAY=5  # seconds before corner can trigger again

# Killer feature: prevent lock when certain programs are running
# Useful for presentations, video calls, etc.
PREVENT_PROGRAMS="zoom teams-for-linux slack firefox-fullscreen chrome-fullscreen"
EOF
        source "$CONFIG_FILE"
    fi
}

# Function to check if lock should be prevented
should_prevent_lock() {
    if [ -n "$PREVENT_PROGRAMS" ]; then
        for prog in $PREVENT_PROGRAMS; do
            # Check for fullscreen windows
            if [[ "$prog" == *"-fullscreen" ]]; then
                base_prog="${prog%-fullscreen}"
                # Check if any window is fullscreen
                if xprop -root _NET_ACTIVE_WINDOW | xargs -I {} xprop -id {} _NET_WM_STATE | grep -q _NET_WM_STATE_FULLSCREEN; then
                    # Check if it's the target program
                    if pgrep -x "$base_prog" > /dev/null; then
                        return 0
                    fi
                fi
            else
                # Just check if program is running
                if pgrep -x "$prog" > /dev/null; then
                    return 0
                fi
            fi
        done
    fi
    return 1
}

# Notification function
notify_lock() {
    if command -v notify-send > /dev/null 2>&1; then
        notify-send -u critical -t $((NOTIFY_TIME * 60 * 1000)) \
            "Screen Lock Warning" \
            "Screen will lock in $NOTIFY_TIME minute(s) due to inactivity"
    fi
}

# Start xautolock with configuration
start_autolock() {
    load_config
    
    # Kill any existing xautolock instance
    pkill -x xautolock 2>/dev/null
    
    # Build xautolock command
    XAUTOLOCK_CMD="xautolock"
    
    # Set idle time
    XAUTOLOCK_CMD="$XAUTOLOCK_CMD -time $IDLE_TIME"
    
    # Set lock command based on method
    if [ "$LOCK_METHOD" = "wallpaper" ]; then
        XAUTOLOCK_CMD="$XAUTOLOCK_CMD -locker '$LOCK_SCRIPT --wallpaper'"
    else
        XAUTOLOCK_CMD="$XAUTOLOCK_CMD -locker '$LOCK_SCRIPT'"
    fi
    
    # Add notification if enabled
    if [ "$NOTIFY_TIME" -gt 0 ]; then
        XAUTOLOCK_CMD="$XAUTOLOCK_CMD -notify $((NOTIFY_TIME * 60))"
        XAUTOLOCK_CMD="$XAUTOLOCK_CMD -notifier '$0 notify'"
    fi
    
    # Add corners if enabled
    if [ "$CORNERS_ENABLED" = "true" ]; then
        XAUTOLOCK_CMD="$XAUTOLOCK_CMD -corners +00-"  # Top-left triggers, bottom-right prevents
        XAUTOLOCK_CMD="$XAUTOLOCK_CMD -cornerdelay $CORNER_DELAY"
        XAUTOLOCK_CMD="$XAUTOLOCK_CMD -cornerredelay $CORNER_REDELAY"
    fi
    
    # Add secure mode (lock even if pointer is grabbed)
    XAUTOLOCK_CMD="$XAUTOLOCK_CMD -secure"
    
    # Start xautolock
    echo "Starting xautolock with $IDLE_TIME minute timeout..."
    eval "$XAUTOLOCK_CMD" &
    
    echo "Auto-lock started successfully"
    echo "Configuration: $CONFIG_FILE"
    echo "Idle timeout: $IDLE_TIME minutes"
    [ "$NOTIFY_TIME" -gt 0 ] && echo "Warning: $NOTIFY_TIME minute(s) before lock"
}

# Stop xautolock
stop_autolock() {
    pkill -x xautolock
    echo "Auto-lock stopped"
}

# Reload configuration
reload_autolock() {
    echo "Reloading auto-lock configuration..."
    start_autolock
}

# Manual lock (useful for keybinding)
lock_now() {
    if [ "$1" = "wallpaper" ] || [ "$LOCK_METHOD" = "wallpaper" ]; then
        $LOCK_SCRIPT --wallpaper
    else
        $LOCK_SCRIPT
    fi
}

# Temporarily disable auto-lock (useful for presentations)
disable_temporarily() {
    MINUTES="${1:-60}"
    echo "Disabling auto-lock for $MINUTES minutes..."
    xautolock -disable
    (sleep $((MINUTES * 60)) && xautolock -enable && echo "Auto-lock re-enabled") &
}

# Main script logic
case "$1" in
    start)
        start_autolock
        ;;
    stop)
        stop_autolock
        ;;
    reload)
        reload_autolock
        ;;
    lock)
        lock_now "$2"
        ;;
    disable)
        disable_temporarily "$2"
        ;;
    notify)
        notify_lock
        ;;
    status)
        if pgrep -x xautolock > /dev/null; then
            echo "Auto-lock is running"
            load_config
            echo "Idle timeout: $IDLE_TIME minutes"
            echo "Config file: $CONFIG_FILE"
        else
            echo "Auto-lock is not running"
        fi
        ;;
    edit)
        ${EDITOR:-nano} "$CONFIG_FILE"
        echo "Configuration edited. Run '$0 reload' to apply changes"
        ;;
    *)
        echo "XMonad Auto-Lock Manager"
        echo "Usage: $0 {start|stop|reload|lock|disable|status|edit}"
        echo ""
        echo "Commands:"
        echo "  start           Start auto-lock with current configuration"
        echo "  stop            Stop auto-lock"
        echo "  reload          Reload configuration and restart"
        echo "  lock [METHOD]   Lock screen immediately (blur or wallpaper)"
        echo "  disable [MINS]  Temporarily disable for N minutes (default: 60)"
        echo "  status          Show current status"
        echo "  edit            Edit configuration file"
        echo ""
        echo "Configuration: $CONFIG_FILE"
        ;;
esac