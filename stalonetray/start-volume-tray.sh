#!/bin/bash
# Wrapper script to start volume control with proper environment

# Kill any existing instance
pkill pasystray 2>/dev/null

# Set up environment similar to blueman
export GTK_THEME=Adwaita
export XDG_CURRENT_DESKTOP=GNOME
export GTK_ICON_THEME=Adwaita
export GTK2_RC_FILES=/usr/share/themes/Adwaita/gtk-2.0/gtkrc

# Try to ensure icon theme is available
if [ -d /usr/share/icons/Adwaita ]; then
    export GTK_PATH=/usr/share/icons/Adwaita:$GTK_PATH
fi

# Start pasystray
exec pasystray "$@"