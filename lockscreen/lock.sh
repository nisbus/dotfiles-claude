#!/bin/bash
# Lock screen script for XMonad
# Based on gh0stzk's ScreenLocker, adapted for XMonad

TEMP_IMAGE="/tmp/xmonad-lock.jpg"
SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"

# Colors (can be customized per theme)
BG="1a1a1a"
FG="d0d0d0"
RING="2a2a2a"
WRONG="bf616a"
DATE="d0d0d0"
VERIFY="a3be8c"

# Function to take screenshot and blur it
blur_lock() {
    # Check if maim is installed
    if ! command -v maim &> /dev/null; then
        echo "Installing maim for screenshots..."
        sudo dnf install -y maim imagemagick
    fi
    
    # Check if i3lock is installed
    if ! command -v i3lock &> /dev/null; then
        echo "Installing i3lock..."
        sudo dnf install -y i3lock
    fi
    
    # Check if xset is installed (needed for DPMS control)
    if ! command -v xset &> /dev/null; then
        echo "Installing xset for display power management..."
        sudo dnf install -y xset
    fi
    
    # Take screenshot and blur it
    maim -d 0.1 "$TEMP_IMAGE"
    
    # Apply blur effect if imagemagick is available
    if command -v convert &> /dev/null; then
        convert "$TEMP_IMAGE" -blur 0x8 "$TEMP_IMAGE"
    elif command -v magick &> /dev/null; then
        magick "$TEMP_IMAGE" -blur 0x8 "$TEMP_IMAGE"
    fi
    
    # Disable DPMS and screen blanking before locking
    if [ -x "$SCRIPT_DIR/dpms-control.sh" ]; then
        "$SCRIPT_DIR/dpms-control.sh" disable
    else
        if command -v xset &> /dev/null; then
            xset s off
            xset -dpms
            xset s noblank
        fi
    fi
    
    # Lock with i3lock (standard version)
    # Using basic i3lock options that are widely supported
    i3lock -n -i "$TEMP_IMAGE" -e -t
    
    # Re-enable DPMS after unlocking
    if [ -x "$SCRIPT_DIR/dpms-control.sh" ]; then
        "$SCRIPT_DIR/dpms-control.sh" restore
    else
        if command -v xset &> /dev/null; then
            xset s on
            xset +dpms
        fi
    fi
}

# Function to use a wallpaper for lock screen
wallpaper_lock() {
    WALLPAPER="$1"
    
    # Check if xset is installed (needed for DPMS control)
    if ! command -v xset &> /dev/null; then
        echo "Installing xset for display power management..."
        sudo dnf install -y xset
    fi
    
    if [ ! -f "$WALLPAPER" ]; then
        echo "Wallpaper not found, using blur lock instead"
        blur_lock
        return
    fi
    
    # Convert wallpaper to jpg if needed
    case "$WALLPAPER" in
        *.webp|*.png)
            if command -v convert &> /dev/null; then
                convert "$WALLPAPER" "$TEMP_IMAGE"
            elif command -v magick &> /dev/null; then
                magick "$WALLPAPER" "$TEMP_IMAGE"
            else
                cp "$WALLPAPER" "$TEMP_IMAGE"
            fi
            ;;
        *.jpg|*.jpeg)
            cp "$WALLPAPER" "$TEMP_IMAGE"
            ;;
        *)
            echo "Unsupported image format, using blur lock"
            blur_lock
            return
            ;;
    esac
    
    # Disable DPMS and screen blanking before locking
    if [ -x "$SCRIPT_DIR/dpms-control.sh" ]; then
        "$SCRIPT_DIR/dpms-control.sh" disable
    else
        if command -v xset &> /dev/null; then
            xset s off
            xset -dpms
            xset s noblank
        fi
    fi
    
    # Lock with wallpaper (standard i3lock version)
    i3lock -n -i "$TEMP_IMAGE" -e -t
    
    # Re-enable DPMS after unlocking
    if [ -x "$SCRIPT_DIR/dpms-control.sh" ]; then
        "$SCRIPT_DIR/dpms-control.sh" restore
    else
        if command -v xset &> /dev/null; then
            xset s on
            xset +dpms
        fi
    fi
}

# Main logic
case "$1" in
    -w|--wallpaper)
        if [ -n "$2" ]; then
            wallpaper_lock "$2"
        else
            # Try to use current wallpaper
            if [ -f "$HOME/.fehbg" ]; then
                CURRENT_WALL=$(grep -oP "(?<=').*(?=')" "$HOME/.fehbg" | head -1)
                if [ -f "$CURRENT_WALL" ]; then
                    wallpaper_lock "$CURRENT_WALL"
                else
                    blur_lock
                fi
            else
                blur_lock
            fi
        fi
        ;;
    -h|--help)
        echo "XMonad Lock Screen"
        echo "Usage: $0 [options]"
        echo ""
        echo "Options:"
        echo "  -w, --wallpaper [FILE]  Use wallpaper for lock screen"
        echo "  -h, --help             Show this help message"
        echo ""
        echo "Without options, takes a screenshot and blurs it"
        ;;
    *)
        blur_lock
        ;;
esac