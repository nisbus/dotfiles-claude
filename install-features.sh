#!/bin/bash
# Complete installation script for XMonad features and theming

set -e  # Exit on error

# Load utility functions
source "$(dirname "$0")/utils.sh"

echo "==================================="
echo "XMonad Features Installation Script"
echo "==================================="

echo "Installing system packages..."

# Install core packages
install_packages picom dunst rofi i3lock xautolock flameshot maim xdotool xsettingsd arc-theme papirus-icon-theme gnome-themes-extra

# Install development tools for building EWW
install_packages build-essential libgtk-3-dev libglib2.0-dev libwebkit2gtk-4.1-dev pkg-config libdbusmenu-glib-dev libdbusmenu-gtk3-dev

# Install Rust (for EWW) if not present
if ! command -v cargo &> /dev/null; then
    echo "Installing Rust..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    source "$HOME/.cargo/env"
fi

# Skip EWW installation for now (can be built manually if needed)
echo "Skipping EWW (Elkowar's Wacky Widgets) installation..."
echo "EWW can be installed manually later with the following commands:"
echo "  git clone https://github.com/elkowar/eww"
echo "  cd eww && cargo build --release --no-default-features --features wayland"
echo "  sudo cp target/release/eww /usr/local/bin/"

# Create config directories
echo "Creating configuration directories..."
mkdir -p ~/.config/{picom,dunst,eww,xmonad,gtk-3.0}
mkdir -p ~/.config/environment.d

# Copy configurations
echo "Copying configuration files..."

# Copy EWW config
if [ -d "eww" ]; then
    cp -r eww/* ~/.config/eww/
fi

# Copy scripts
if [ -d "scripts" ]; then
    cp -r scripts ~/.config/xmonad/
    chmod +x ~/.config/xmonad/scripts/*.sh
fi

# Copy theme-switcher
if [ -d "theme-switcher" ]; then
    cp -r theme-switcher ~/.config/xmonad/
    chmod +x ~/.config/xmonad/theme-switcher/*.sh
fi

# Copy lockscreen
if [ -d "lockscreen" ]; then
    cp -r lockscreen ~/.config/xmonad/
    chmod +x ~/.config/xmonad/lockscreen/*.sh
fi

# Copy scratchpad
if [ -d "scratchpad" ]; then
    cp -r scratchpad ~/.config/xmonad/
fi

# Setup themes
echo "Setting up themes..."
THEMES_DIR="$HOME/.config/xmonad/themes"
mkdir -p "$THEMES_DIR"

# Copy cristina and silvia themes
for theme in cristina silvia; do
    if [ ! -d "$THEMES_DIR/$theme" ]; then
        echo "Creating $theme theme..."
        mkdir -p "$THEMES_DIR/$theme"
        
        # You'll need to have these theme files in your repo
        # or create them based on the color schemes
    fi
done

# Create default picom config
cat > ~/.config/picom/picom.conf << 'EOF'
# Picom configuration for XMonad
backend = "glx";
glx-no-stencil = true;
glx-copy-from-front = false;

# Opacity
inactive-opacity = 1;
active-opacity = 1;
frame-opacity = 0.9;
inactive-opacity-override = false;

# Opacity rules
opacity-rule = [
    "85:class_g = 'firefox'",
    "85:class_g = 'Firefox'",
    "85:class_g = 'Navigator'",
    "85:class_g = 'Org.gnome.Nautilus'",
    "85:class_g = 'nautilus'",
    "85:class_g = 'Nautilus'",
    "90:class_g = 'Code'",
    "100:class_g = 'xmobar'",
    "100:class_g = 'dmenu'",
    "100:class_g = 'rofi'",
    "100:class_g = 'eww-sliders'",
    "100:class_g = 'eww-system_info'",
    "100:class_g = 'eww-power_menu'",
    "100:class_g = 'Eww'",
    "95:class_g = 'Dunst'"
];

# Fading
fading = true;
fade-delta = 4;
fade-in-step = 0.03;
fade-out-step = 0.03;

# Shadows
shadow = true;
shadow-radius = 12;
shadow-offset-x = -12;
shadow-offset-y = -12;
shadow-opacity = 0.7;

shadow-exclude = [
    "name = 'Notification'",
    "class_g = 'eww-sliders'",
    "class_g = 'eww-system_info'",
    "class_g = 'eww-power_menu'",
    "_GTK_FRAME_EXTENTS@:c"
];

# Rounded corners
corner-radius = 10;
rounded-corners-exclude = [
    "class_g = 'xmobar'",
    "class_g = 'dmenu'"
];

# Other settings
mark-wmwin-focused = true;
mark-ovredir-focused = true;
detect-rounded-corners = true;
detect-client-opacity = true;
vsync = true;
detect-transient = true;
detect-client-leader = true;

wintypes:
{
    tooltip = { fade = true; shadow = true; opacity = 0.95; focus = true; full-shadow = false; };
    dock = { shadow = false; }
    dnd = { shadow = false; }
    popup_menu = { opacity = 0.95; }
    dropdown_menu = { opacity = 0.95; }
};
EOF

# Create dunst config directory
mkdir -p ~/.config/dunst

# Create xsettingsd config
cat > ~/.xsettingsd << 'EOF'
Net/ThemeName "Arc-Dark"
Net/IconThemeName "Papirus-Dark"
Gtk/CursorThemeName "Adwaita"
Net/EnableEventSounds 0
Net/EnableInputFeedbackSounds 0
Gtk/FontName "JetBrains Mono 11"
Xft/Antialias 1
Xft/Hinting 1
Xft/HintStyle "hintmedium"
Xft/RGBA "rgb"
EOF

# Create GTK theme environment
cat > ~/.config/environment.d/gtk.conf << 'EOF'
GTK_THEME=Arc-Dark
GTK2_RC_FILES=/home/$USER/.gtkrc-2.0
EOF

# Set default theme
echo "cristina" > ~/.config/xmonad/.current-theme

echo ""
echo "==================================="
echo "Installation Complete!"
echo "==================================="
echo ""
echo "Next steps:"
echo "1. Restart XMonad (Super+q) to load all features"
echo "2. Press Super+t to switch between Cristina and Silvia themes"
echo "3. Press Super+i to toggle EWW widgets"
echo "4. Press Super+F1 to see the keybindings cheatsheet"
echo ""
echo "Make sure to add these to your .xmonad/xmonad.hs startup:"
echo "  spawn \"picom -b --config ~/.config/picom/picom.conf\""
echo "  spawn \"dunst &\""
echo "  spawn \"xsettingsd &\""
echo "  spawn \"eww daemon\""
echo ""