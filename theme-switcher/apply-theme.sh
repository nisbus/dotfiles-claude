#!/bin/bash
# Apply theme script for XMonad
# Loads theme configuration and applies it to various components

THEME_NAME="$1"
THEMES_DIR="$HOME/.config/xmonad/themes"
THEME_DIR="$THEMES_DIR/$THEME_NAME"

if [ ! -d "$THEME_DIR" ]; then
    echo "Error: Theme '$THEME_NAME' not found!"
    exit 1
fi

# Source theme colors
if [ -f "$THEME_DIR/colors.conf" ]; then
    source "$THEME_DIR/colors.conf"
fi

# Apply to xmobar if colors are defined
if [ -n "$BACKGROUND" ] && [ -n "$FOREGROUND" ]; then
    # Update xmobar colors
    XMOBAR_CONFIG="$HOME/.xmobarrc"
    if [ -f "$XMOBAR_CONFIG" ]; then
        # Create backup
        cp "$XMOBAR_CONFIG" "$XMOBAR_CONFIG.bak"
        
        # Update colors in xmobar config
        sed -i "s/bgColor = \".*\"/bgColor = \"$BACKGROUND\"/" "$XMOBAR_CONFIG"
        sed -i "s/fgColor = \".*\"/fgColor = \"$FOREGROUND\"/" "$XMOBAR_CONFIG"
    fi
fi

# Apply to alacritty if theme file exists
if [ -f "$THEME_DIR/alacritty.toml" ]; then
    ALACRITTY_CONFIG="$HOME/.config/alacritty/alacritty.toml"
    ALACRITTY_THEME_DIR="$HOME/.config/alacritty/themes"
    
    mkdir -p "$ALACRITTY_THEME_DIR"
    cp "$THEME_DIR/alacritty.toml" "$ALACRITTY_THEME_DIR/current.toml"
    
    # Update alacritty config to import current theme
    if [ -f "$ALACRITTY_CONFIG" ]; then
        if ! grep -q "themes/current.toml" "$ALACRITTY_CONFIG"; then
            echo 'import = ["~/.config/alacritty/themes/current.toml"]' >> "$ALACRITTY_CONFIG"
        fi
    fi
fi

# Apply wallpaper if exists
if [ -f "$THEME_DIR/wallpaper.jpg" ] || [ -f "$THEME_DIR/wallpaper.png" ]; then
    WALLPAPER=$(find "$THEME_DIR" -name "wallpaper.*" | head -1)
    if [ -n "$WALLPAPER" ]; then
        feh --bg-scale "$WALLPAPER"
        # Save wallpaper setting for persistence
        echo "feh --bg-scale '$WALLPAPER'" > "$HOME/.fehbg"
        chmod +x "$HOME/.fehbg"
    fi
fi

# Apply GTK theme if specified
if [ -f "$THEME_DIR/gtk.conf" ]; then
    source "$THEME_DIR/gtk.conf"
    if [ -n "$GTK_THEME" ]; then
        # Update GTK3 settings
        GTK3_CONFIG="$HOME/.config/gtk-3.0/settings.ini"
        mkdir -p "$(dirname "$GTK3_CONFIG")"
        
        if [ -f "$GTK3_CONFIG" ]; then
            sed -i "s/gtk-theme-name=.*/gtk-theme-name=$GTK_THEME/" "$GTK3_CONFIG"
        else
            cat > "$GTK3_CONFIG" << EOF
[Settings]
gtk-theme-name=$GTK_THEME
gtk-icon-theme-name=Adwaita
gtk-font-name=Sans 10
EOF
        fi
    fi
fi

echo "Theme '$THEME_NAME' applied successfully!"