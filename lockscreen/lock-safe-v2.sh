#!/bin/bash
# Enhanced safe lock screen with visual feedback
# Shows password input indicator and typing feedback

TEMP_IMAGE="/tmp/xmonad-lock-safe.png"
LOCK_PID_FILE="/tmp/i3lock.pid"

# Function to ensure dependencies
check_dependencies() {
    # Check if i3lock-color is available (better UI)
    if command -v i3lock-color &> /dev/null; then
        echo "Using i3lock-color for enhanced UI"
        return 0
    fi
    
    # Try to install i3lock-color
    echo "Installing i3lock-color for better lock screen UI..."
    if command -v dnf &> /dev/null; then
        sudo dnf install -y i3lock-color || {
            echo "Could not install i3lock-color, trying i3lock"
            sudo dnf install -y i3lock
        }
    fi
    
    # Check for screenshot tool
    if ! command -v scrot &> /dev/null; then
        sudo dnf install -y scrot
    fi
}

# Function to take and blur screenshot
prepare_background() {
    # Take screenshot
    scrot -z -q 1 "$TEMP_IMAGE"
    
    # Add blur and darkening effect
    if command -v convert &> /dev/null; then
        # Blur and darken the image for better visibility of the lock UI
        convert "$TEMP_IMAGE" \
            -blur 0x8 \
            -brightness-contrast -30x-20 \
            -fill black -colorize 40% \
            "$TEMP_IMAGE"
        
        # Add a lock icon or text in the center (optional)
        convert "$TEMP_IMAGE" \
            -gravity center \
            -pointsize 48 \
            -fill white \
            -annotate +0-100 "🔒" \
            -pointsize 24 \
            -annotate +0+0 "Enter Password to Unlock" \
            "$TEMP_IMAGE"
    fi
}

# Main lock function
lock_with_ui() {
    # Check dependencies
    check_dependencies
    
    # Check if already locked
    if pgrep -x i3lock > /dev/null; then
        echo "Lock screen already active"
        exit 1
    fi
    
    # Disable DPMS to prevent screen sleep
    xset s off
    xset -dpms
    xset s noblank
    
    # Prepare background
    prepare_background
    
    # Determine which i3lock to use
    if command -v i3lock-color &> /dev/null; then
        # Use i3lock-color with full UI
        i3lock-color \
            --image="$TEMP_IMAGE" \
            --radius=120 \
            --ring-width=10 \
            --indicator \
            --clock \
            --time-str="%H:%M:%S" \
            --date-str="%A, %B %d, %Y" \
            --time-color=ffffffff \
            --date-color=ffffffff \
            --inside-color=00000088 \
            --ring-color=ffffffaa \
            --line-color=00000000 \
            --keyhl-color=88c0d0ff \
            --ringver-color=a3be8cff \
            --insidever-color=a3be8c88 \
            --ringwrong-color=bf616aff \
            --insidewrong-color=bf616a88 \
            --verif-text="Checking..." \
            --wrong-text="Wrong Password!" \
            --noinput-text="No Input" \
            --lock-text="Locking..." \
            --lockfailed-text="Lock Failed!" \
            --pass-media-keys \
            --pass-screen-keys \
            --pass-power-keys \
            --pass-volume-keys \
            --verif-color=ffffffff \
            --wrong-color=bf616aff \
            --modif-color=ffffffff \
            --show-failed-attempts \
            --ignore-empty-password \
            --nofork
            
    elif command -v i3lock &> /dev/null; then
        # Fallback to standard i3lock with basic options
        # Note: Standard i3lock has limited UI options
        echo "Using basic i3lock (limited UI feedback)"
        i3lock \
            --nofork \
            --image="$TEMP_IMAGE" \
            --ignore-empty-password \
            --show-failed-attempts \
            --tiling \
            --pointer=default \
            --color=000000
    else
        echo "Error: No lock program found!"
        exit 1
    fi
    
    # Clean up after unlock
    rm -f "$TEMP_IMAGE"
    
    # Re-enable DPMS with safe values
    xset s on
    xset +dpms
    xset dpms 7200 7200 7200  # 2 hours
}

# Handle signals gracefully
trap 'rm -f "$TEMP_IMAGE"; exit' INT TERM

# Execute
lock_with_ui