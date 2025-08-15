#!/bin/bash
# Scratchpad terminal for XMonad
# Based on gh0stzk's Bspwm-ScratchPad, adapted for XMonad

SCRATCHPAD_NAME="xmonad-scratchpad"
TERMINAL="alacritty"
WIDTH_PERCENT=70
HEIGHT_PERCENT=35

# Get screen dimensions using xrandr
get_screen_geometry() {
    # Get primary monitor dimensions
    SCREEN_INFO=$(xrandr | grep "primary" | grep -oP '\d+x\d+\+\d+\+\d+')
    if [ -z "$SCREEN_INFO" ]; then
        # Fallback to first connected monitor
        SCREEN_INFO=$(xrandr | grep " connected" | head -1 | grep -oP '\d+x\d+\+\d+\+\d+')
    fi
    
    # Parse dimensions
    SCREEN_WIDTH=$(echo "$SCREEN_INFO" | cut -d'x' -f1)
    SCREEN_HEIGHT=$(echo "$SCREEN_INFO" | cut -d'x' -f2 | cut -d'+' -f1)
    SCREEN_X=$(echo "$SCREEN_INFO" | cut -d'+' -f2)
    SCREEN_Y=$(echo "$SCREEN_INFO" | cut -d'+' -f3)
}

# Calculate position and size
calculate_geometry() {
    WIDTH=$((SCREEN_WIDTH * WIDTH_PERCENT / 100))
    HEIGHT=$((SCREEN_HEIGHT * HEIGHT_PERCENT / 100))
    X_POS=$((SCREEN_X + (SCREEN_WIDTH - WIDTH) / 2))
    Y_POS=$SCREEN_Y
}

# Find scratchpad window
find_scratchpad() {
    xdotool search --classname "$SCRATCHPAD_NAME" 2>/dev/null | head -1
}

# Toggle scratchpad visibility
toggle_scratchpad() {
    WINDOW_ID=$(find_scratchpad)
    
    if [ -n "$WINDOW_ID" ]; then
        # Window exists, check if visible
        if xdotool search --onlyvisible --classname "$SCRATCHPAD_NAME" 2>/dev/null | grep -q "$WINDOW_ID"; then
            # Hide window
            xdotool windowunmap "$WINDOW_ID"
        else
            # Show and focus window
            xdotool windowmap "$WINDOW_ID"
            xdotool windowfocus "$WINDOW_ID"
            xdotool windowraise "$WINDOW_ID"
            
            # Reposition and resize
            get_screen_geometry
            calculate_geometry
            xdotool windowmove "$WINDOW_ID" "$X_POS" "$Y_POS"
            xdotool windowsize "$WINDOW_ID" "$WIDTH" "$HEIGHT"
        fi
    else
        # Create new scratchpad terminal
        get_screen_geometry
        calculate_geometry
        
        case "$TERMINAL" in
            alacritty)
                alacritty --class "$SCRATCHPAD_NAME,$SCRATCHPAD_NAME" \
                    --title "Scratchpad" \
                    -o window.position.x="$X_POS" \
                    -o window.position.y="$Y_POS" \
                    -o window.dimensions.columns=$((WIDTH / 8)) \
                    -o window.dimensions.lines=$((HEIGHT / 16)) &
                ;;
            kitty)
                kitty --class="$SCRATCHPAD_NAME" \
                    --title="Scratchpad" \
                    --override initial_window_width="${WIDTH}" \
                    --override initial_window_height="${HEIGHT}" &
                ;;
            *)
                xterm -class "$SCRATCHPAD_NAME" \
                    -title "Scratchpad" \
                    -geometry "${WIDTH}x${HEIGHT}+${X_POS}+${Y_POS}" &
                ;;
        esac
        
        # Wait for window to appear and set it as floating
        sleep 0.3
        WINDOW_ID=$(find_scratchpad)
        if [ -n "$WINDOW_ID" ]; then
            # Make window floating (for XMonad, this is handled in config)
            # Set window type to dialog to make it float
            xprop -id "$WINDOW_ID" -f _NET_WM_WINDOW_TYPE 32a \
                -set _NET_WM_WINDOW_TYPE _NET_WM_WINDOW_TYPE_DIALOG
            
            # Position and size the window
            xdotool windowmove "$WINDOW_ID" "$X_POS" "$Y_POS"
            xdotool windowsize "$WINDOW_ID" "$WIDTH" "$HEIGHT"
            xdotool windowfocus "$WINDOW_ID"
        fi
    fi
}

# Install dependencies if needed
check_dependencies() {
    if ! command -v xdotool &> /dev/null; then
        echo "Installing xdotool..."
        sudo dnf install -y xdotool
    fi
    
    if ! command -v xprop &> /dev/null; then
        echo "Installing xprop..."
        sudo dnf install -y xorg-x11-utils
    fi
}

# Main
case "$1" in
    -h|--help)
        echo "XMonad Scratchpad Terminal"
        echo "Usage: $0 [options]"
        echo ""
        echo "Options:"
        echo "  -h, --help    Show this help message"
        echo ""
        echo "Toggles a floating terminal window (scratchpad)"
        ;;
    *)
        check_dependencies
        toggle_scratchpad
        ;;
esac