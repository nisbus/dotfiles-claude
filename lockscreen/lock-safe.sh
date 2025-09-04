#!/bin/bash
# Safe lock screen script that prevents input deadlock
# This uses i3lock with careful handling to avoid the frozen input issue

TEMP_IMAGE="/tmp/xmonad-lock-safe.png"
ICON_IMAGE="/tmp/lock-icon.png"
FINAL_IMAGE="/tmp/xmonad-lock-final.png"
LOCK_PID_FILE="/tmp/i3lock.pid"

# Function to ensure dependencies
check_dependencies() {
    local missing=""
    
    # Check for required tools
    for cmd in i3lock scrot; do
        if ! command -v $cmd &> /dev/null; then
            missing="$missing $cmd"
        fi
    done
    
    if [ -n "$missing" ]; then
        echo "Installing required packages:$missing"
        sudo dnf install -y i3lock scrot
    fi
    
    # Check for ImageMagick (either version)
    if ! command -v magick &> /dev/null && ! command -v convert &> /dev/null; then
        echo "Installing ImageMagick..."
        sudo dnf install -y ImageMagick
    fi
}

# Function to detect ImageMagick command
get_magick_cmd() {
    if command -v magick &> /dev/null; then
        echo "magick"
    else
        echo "convert"
    fi
}

# Function to create lock icon
create_lock_icon() {
    local MAGICK=$(get_magick_cmd)
    
    # Create a smaller, better positioned lock icon
    $MAGICK -size 200x150 xc:transparent \
        -gravity center \
        -fill white \
        -font "DejaVu-Sans" \
        -pointsize 48 \
        -annotate +0-15 "🔒" \
        -pointsize 16 \
        -fill "#ffffff" \
        -annotate +0+25 "Enter Password" \
        -pointsize 12 \
        -fill "#cccccc" \
        -annotate +0+45 "$(date '+%H:%M')" \
        "$ICON_IMAGE"
}

# Function to prepare lock screen background
prepare_background() {
    echo "Preparing lock screen..."
    local MAGICK=$(get_magick_cmd)
    
    # Take screenshot of all monitors
    scrot -m -z -q 1 "$TEMP_IMAGE"
    
    if command -v $MAGICK &> /dev/null; then
        # Create lock icon
        create_lock_icon
        
        # Get primary monitor info for proper positioning
        local PRIMARY_RES=$(xrandr | grep "primary" | grep -oP '\d+x\d+' | head -1)
        local PRIMARY_W=$(echo $PRIMARY_RES | cut -dx -f1)
        local PRIMARY_H=$(echo $PRIMARY_RES | cut -dx -f2)
        
        # If no primary found, use first connected monitor
        if [ -z "$PRIMARY_W" ]; then
            PRIMARY_RES=$(xrandr | grep " connected" | head -1 | grep -oP '\d+x\d+' | head -1)
            PRIMARY_W=$(echo $PRIMARY_RES | cut -dx -f1)
            PRIMARY_H=$(echo $PRIMARY_RES | cut -dx -f2)
        fi
        
        # Apply effects to screenshot:
        # 1. Blur the image
        # 2. Darken it  
        # 3. Add the lock icon only on primary monitor area
        $MAGICK "$TEMP_IMAGE" \
            -blur 0x8 \
            -brightness-contrast -30x-20 \
            -fill black -colorize 30% \
            "$FINAL_IMAGE"
        
        # Calculate center position for primary monitor
        local CENTER_X=$((PRIMARY_W / 2))
        local CENTER_Y=$((PRIMARY_H / 2))
        
        # Add lock icon at center of primary monitor
        $MAGICK "$FINAL_IMAGE" \
            "$ICON_IMAGE" -geometry +$((CENTER_X - 100))+$((CENTER_Y - 75)) -composite \
            "$FINAL_IMAGE"
        
        # Add typing hint at safe position (avoiding edges)
        $MAGICK "$FINAL_IMAGE" \
            -fill white -pointsize 12 \
            -annotate +$CENTER_X+$((CENTER_Y + 100)) "Start typing • Press Enter to unlock" \
            "$FINAL_IMAGE"
            
        # Use the final image
        mv "$FINAL_IMAGE" "$TEMP_IMAGE"
    else
        # Fallback - just blur
        $MAGICK "$TEMP_IMAGE" -blur 0x8 "$TEMP_IMAGE" 2>/dev/null || true
    fi
    
    # Clean up
    rm -f "$ICON_IMAGE"
}

# Main lock function
safe_lock() {
    # Check if already locked
    if pgrep -x i3lock > /dev/null; then
        echo "Lock screen already active"
        exit 1
    fi
    
    # Check dependencies
    check_dependencies
    
    # CRITICAL: Disable DPMS before locking to prevent screen sleep issues
    xset s off
    xset -dpms
    xset s noblank
    
    # Prepare the lock screen background
    prepare_background
    
    # Lock the screen with i3lock
    # Using minimal options for standard i3lock
    # -n: Don't fork (wait for unlock)
    # -i: Use our prepared image
    # -e: Ignore empty password
    # -p default: Show default mouse pointer
    # -u: Don't show unlock indicator (removes the confusing NumLock text)
    echo "Locking screen..."
    i3lock \
        --nofork \
        --image="$TEMP_IMAGE" \
        --ignore-empty-password \
        --pointer=default \
        --no-unlock-indicator
    
    # After unlock, clean up
    rm -f "$TEMP_IMAGE" "$FINAL_IMAGE"
    
    # Re-enable DPMS with safe values
    xset s on
    xset +dpms
    xset dpms 7200 7200 7200  # 2 hours to prevent issues
    
    echo "Screen unlocked"
}

# Handle signals gracefully
trap 'rm -f "$TEMP_IMAGE" "$ICON_IMAGE" "$FINAL_IMAGE"; exit' INT TERM

# Execute
safe_lock