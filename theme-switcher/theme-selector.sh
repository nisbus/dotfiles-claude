#!/bin/bash
# Theme Selector for XMonad
# Based on gh0stzk's RiceSelector, adapted for XMonad themes

# Configuration directory
THEMES_DIR="$HOME/.config/xmonad/themes"
CURRENT_THEME_FILE="$HOME/.config/xmonad/.current-theme"

# Create themes directory if it doesn't exist
mkdir -p "$THEMES_DIR"

# Get current theme
if [ -f "$CURRENT_THEME_FILE" ]; then
    CURRENT_THEME=$(cat "$CURRENT_THEME_FILE")
else
    CURRENT_THEME="default"
    echo "$CURRENT_THEME" > "$CURRENT_THEME_FILE"
fi

# List available themes
THEMES=$(find "$THEMES_DIR" -mindepth 1 -maxdepth 1 -type d -exec basename {} \; | sort)

# If no themes exist, create a default one
if [ -z "$THEMES" ]; then
    mkdir -p "$THEMES_DIR/default"
    cat > "$THEMES_DIR/default/colors.conf" << 'EOF'
# Default theme colors
BACKGROUND="#1a1a1a"
FOREGROUND="#d0d0d0"
ACCENT="#5e81ac"
BORDER_NORMAL="#cccccc"
BORDER_FOCUSED="#cd8b00"
EOF
    THEMES="default"
fi

# Show selection menu using dmenu (simpler than rofi for now)
SELECTED=$(echo "$THEMES" | dmenu -i -p "Select Theme:" -l 10)

# If a valid option was selected and it's different from current
if [ -n "$SELECTED" ] && [ "$SELECTED" != "$CURRENT_THEME" ]; then
    echo "$SELECTED" > "$CURRENT_THEME_FILE"
    
    # Apply the theme
    "$HOME/.config/xmonad/theme-switcher/apply-theme.sh" "$SELECTED"
    
    # Restart XMonad to apply changes
    xmonad --restart
    
    # Notify user
    notify-send "Theme Switcher" "Theme changed to: $SELECTED" -i preferences-desktop-theme
fi