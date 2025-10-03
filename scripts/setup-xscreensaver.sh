#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOTFILES_DIR="$(dirname "$SCRIPT_DIR")"

detect_os() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        echo "$ID"
    elif [ -f /etc/fedora-release ]; then
        echo "fedora"
    elif [ -f /etc/lsb-release ]; then
        . /etc/lsb-release
        echo "$DISTRIB_ID" | tr '[:upper:]' '[:lower:]'
    else
        echo "unknown"
    fi
}

install_xscreensaver() {
    local os_type=$(detect_os)
    
    echo "=== Installing XScreensaver for $os_type ==="
    
    case "$os_type" in
        fedora)
            sudo dnf install -y xscreensaver xscreensaver-extras xscreensaver-gl-extras
            ;;
        ubuntu|debian)
            sudo apt-get update
            sudo apt-get install -y xscreensaver xscreensaver-data-extra xscreensaver-gl-extra
            ;;
        *)
            echo "Unsupported OS: $os_type"
            exit 1
            ;;
    esac
}

cleanup_old_screensaver() {
    echo "=== Cleaning up old screensaver configuration ==="
    
    pkill -f xautolock 2>/dev/null || true
    pkill -f xss-lock 2>/dev/null || true
    pkill -f xidlehook 2>/dev/null || true
    pkill -f light-locker 2>/dev/null || true
    pkill -f gnome-screensaver 2>/dev/null || true
    
    systemctl --user stop xss-lock.service 2>/dev/null || true
    systemctl --user disable xss-lock.service 2>/dev/null || true
    
    gsettings set org.gnome.desktop.screensaver idle-activation-enabled false 2>/dev/null || true
    gsettings set org.gnome.desktop.lockdown disable-lock-screen false 2>/dev/null || true
    
    if [ -f /tmp/xss-lock.pid ]; then
        rm -f /tmp/xss-lock.pid
    fi
    
    echo "✓ Cleaned up old screensaver processes"
}

configure_xscreensaver() {
    echo "=== Configuring XScreensaver ==="
    
    mkdir -p "$HOME/.config/xscreensaver"
    
    cat > "$HOME/.xscreensaver" << 'EOF'
mode:           one
selected:       -1
timeout:        0:10:00
cycle:          0:10:00
lock:           True
lockTimeout:    0:00:00
passwdTimeout:  0:00:30
visualID:       default
installColormap: True
verbose:        False
splash:         False
splashDuration: 0:00:05
demoCommand:    xscreensaver-command -demo
nice:           10
fade:           True
unfade:         False
fadeSeconds:    0:00:03
fadeTicks:      20
dpmsEnabled:    True
dpmsQuickOff:   False
dpmsStandby:    0:10:00
dpmsSuspend:    0:10:00
dpmsOff:        0:10:00
grabDesktopImages: False
grabVideoFrames:   False
chooseRandomImages: False
imageDirectory:     

programs:                                                                     \
                maze -root                                  \n\
                glmatrix -root                              \n\
                bsod -root                                  \n\
-               apple2 -root                                \n\
                pipes -root                                 \n\
-               xlyap -root                                 \n\
                starwars -root                              \n\
                glslideshow -root                           \n

pointerHysteresis: 10
authWarningSlack: 20

EOF
    
    echo "✓ Created XScreensaver configuration"
}

update_xinitrc() {
    echo "=== Updating xinitrc to start XScreensaver ==="
    
    local xinitrc="$DOTFILES_DIR/xsessions/xinitrc"
    
    if [ ! -f "$xinitrc" ]; then
        echo "Error: xinitrc not found at $xinitrc"
        return 1
    fi
    
    cp "$xinitrc" "${xinitrc}.backup-$(date +%Y%m%d-%H%M%S)"
    
    if grep -q "xscreensaver" "$xinitrc" 2>/dev/null; then
        echo "✓ XScreensaver already in xinitrc"
        return 0
    fi
    
    if grep -q "setup-lock-session.sh" "$xinitrc"; then
        sed -i '/setup-lock-session.sh/d' "$xinitrc"
        sed -i '/# Setup automatic screen locking/d' "$xinitrc"
    fi
    
    if grep -q "gsettings set org.gnome.desktop.session" "$xinitrc"; then
        sed -i '/gsettings set org.gnome.desktop.session/d' "$xinitrc"
        sed -i '/gsettings set org.gnome.desktop.screensaver/d' "$xinitrc"
        sed -i '/# Configure GNOME screensaver/d' "$xinitrc"
        sed -i '/# Note: Using GNOME/d' "$xinitrc"
    fi
    
    awk '/^# Start system tray/ && !done {
        print "# Start XScreensaver daemon"
        print "xscreensaver -no-splash &"
        print ""
        done=1
    }
    {print}' "$xinitrc" > "${xinitrc}.tmp" && mv "${xinitrc}.tmp" "$xinitrc"
    
    echo "✓ Updated xinitrc to start XScreensaver"
}

update_xmonad_config() {
    echo "=== Checking XMonad configuration ==="
    
    local xmonad_hs="$DOTFILES_DIR/xmonad/xmonad.hs"
    
    if [ ! -f "$xmonad_hs" ]; then
        echo "⚠ XMonad config not found at $xmonad_hs"
        return 0
    fi
    
    if grep -q "xscreensaver-command" "$xmonad_hs"; then
        echo "✓ XScreensaver keybinding already in xmonad.hs"
    else
        echo "ℹ Consider adding this keybinding to xmonad.hs:"
        echo '  , ((modMask, xK_l), spawn "xscreensaver-command -lock")'
    fi
}

start_xscreensaver_now() {
    echo "=== Starting XScreensaver for current session ==="
    
    pkill -f xscreensaver 2>/dev/null || true
    sleep 1
    
    xset s off
    xset -dpms
    
    xscreensaver -no-splash &
    
    sleep 2
    
    if pgrep -f xscreensaver > /dev/null; then
        echo "✓ XScreensaver started successfully"
    else
        echo "⚠ Failed to start XScreensaver"
        return 1
    fi
}

main() {
    echo "=========================================="
    echo "XScreensaver Setup Script"
    echo "=========================================="
    echo ""
    
    install_xscreensaver
    cleanup_old_screensaver
    configure_xscreensaver
    update_xinitrc
    update_xmonad_config
    start_xscreensaver_now
    
    echo ""
    echo "=========================================="
    echo "Setup Complete!"
    echo "=========================================="
    echo ""
    echo "XScreensaver is now configured:"
    echo "  ✓ Installed XScreensaver"
    echo "  ✓ Removed old screensaver configuration"
    echo "  ✓ Screen will lock after 10 minutes of inactivity"
    echo "  ✓ xinitrc updated to start XScreensaver on login"
    echo "  ✓ XScreensaver running for current session"
    echo ""
    echo "Manual lock commands:"
    echo "  xscreensaver-command -lock      (recommended)"
    echo "  xscreensaver-command -activate  (same as -lock)"
    echo ""
    echo "Configuration commands:"
    echo "  xscreensaver-command -prefs     (open preferences)"
    echo "  xscreensaver-command -demo      (preview screensavers)"
    echo ""
    echo "Check status:"
    echo "  ps aux | grep xscreensaver"
    echo "  xscreensaver-command -time"
    echo ""
}

main "$@"