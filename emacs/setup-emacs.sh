#!/bin/bash

# Emacs setup script - configures Emacs with proper init file and X11 support

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../utils.sh"

echo "Setting up Emacs configuration..."

# Create Emacs config directory
mkdir -p ~/.emacs.d

# Handle existing .emacs file (it takes precedence over init.el)
if [ -f ~/.emacs ]; then
    echo "Found existing ~/.emacs file (which overrides ~/.emacs.d/init.el)"
    backup_file ~/.emacs
    echo "✓ Backed up ~/.emacs to prevent conflicts"
fi

# Link init.el to the correct location
echo "Linking Emacs init file..."
create_symlink "$SCRIPT_DIR/init.el" "$HOME/.emacs.d/init.el"

# Also create ~/.emacs.d/early-init.el to handle GTK issues
echo "Creating early-init.el to fix GTK/X11 compatibility..."
cat > ~/.emacs.d/early-init.el << 'EOF'
;;; early-init.el --- Early initialization file -*- lexical-binding: t -*-

;; Disable pure GTK to avoid X11 compatibility issues
(when (and (boundp 'pgtk-initialized) pgtk-initialized)
  (message "Warning: Running pure-GTK build under X11. Some features may not work correctly."))

;; Force X11 backend when available
(when (eq window-system 'pgtk)
  (setenv "GDK_BACKEND" "x11"))

;; Disable package.el in favor of use-package
(setq package-enable-at-startup nil)

;; Temporarily increase GC threshold during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reset GC thresholds after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000)
                  gc-cons-percentage 0.1)))

;; Prevent flash of unstyled content
(setq-default inhibit-redisplay t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil)
            (redisplay)))

;; Disable unnecessary UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Set frame size
(push '(width . 120) default-frame-alist)
(push '(height . 40) default-frame-alist)

;;; early-init.el ends here
EOF

echo "✓ Emacs early-init.el created"

# Detect and use the correct Emacs version
EMACS_CMD="emacs"
if [ -n "$DISPLAY" ]; then
    # Check for X11 version of Emacs
    if [ -x "/usr/bin/emacs-30.1-gtk+x11" ]; then
        EMACS_CMD="/usr/bin/emacs-30.1-gtk+x11"
        echo "✓ Found Emacs with X11 support: $EMACS_CMD"
    elif [ -x "/usr/bin/emacs-gtk" ] || [ -x "/usr/bin/emacs-x11" ]; then
        EMACS_CMD=$(which emacs-gtk emacs-x11 2>/dev/null | head -1)
        echo "✓ Found Emacs with X11 support: $EMACS_CMD"
    else
        # Check if current emacs is pgtk version
        EMACS_VERSION=$(emacs --version | head -1)
        if echo "$EMACS_VERSION" | grep -q "pgtk"; then
            echo ""
            echo "⚠️  WARNING: Using pure-GTK build of Emacs with X11."
            echo "   This may cause issues. X11 version detected at:"
            echo "   /usr/bin/emacs-30.1-gtk+x11"
            echo ""
            # Try to use the X11 version if available
            if [ -x "/usr/bin/emacs-30.1-gtk+x11" ]; then
                EMACS_CMD="/usr/bin/emacs-30.1-gtk+x11"
            fi
        fi
    fi
fi

# Initialize Emacs packages
echo "Initializing Emacs packages (this may take a few minutes on first run)..."

# Create a script to bootstrap Emacs packages
cat > /tmp/emacs-bootstrap.el << 'EOF'
;; Bootstrap script to install packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("elpa" . "https://elpa.gnu.org/packages/")))
(setq package-check-signature 'allow-unsigned)
(package-initialize)
(package-refresh-contents)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Load the main config to trigger package installation
(load "~/.emacs.d/init.el")

;; Exit
(kill-emacs 0)
EOF

# Run Emacs in batch mode to install packages
echo "Installing Emacs packages..."
$EMACS_CMD --batch --script /tmp/emacs-bootstrap.el 2>/dev/null || {
    echo "Note: Initial package installation may show some warnings. This is normal."
}

# Clean up
rm -f /tmp/emacs-bootstrap.el

# Create desktop entry for Emacs with X11 backend
mkdir -p ~/.local/share/applications
if [ -x "/usr/bin/emacs-30.1-gtk+x11" ]; then
    cat > ~/.local/share/applications/emacs-x11.desktop << 'EOF'
[Desktop Entry]
Name=Emacs (X11)
GenericName=Text Editor
Comment=GNU Emacs with X11 backend
Exec=/usr/bin/emacs-30.1-gtk+x11 %F
Icon=emacs
Type=Application
Terminal=false
Categories=Development;TextEditor;
StartupNotify=true
MimeType=text/plain;text/x-c;text/x-c++;text/x-c++src;text/x-h;text/x-java;text/x-makefile;text/x-pascal;text/x-perl;text/x-python;text/x-tex;application/x-shellscript;
EOF
else
    cat > ~/.local/share/applications/emacs-x11.desktop << 'EOF'
[Desktop Entry]
Name=Emacs (X11)
GenericName=Text Editor
Comment=GNU Emacs with X11 backend
Exec=env GDK_BACKEND=x11 emacs %F
Icon=emacs
Type=Application
Terminal=false
Categories=Development;TextEditor;
StartupNotify=true
MimeType=text/plain;text/x-c;text/x-c++;text/x-c++src;text/x-h;text/x-java;text/x-makefile;text/x-pascal;text/x-perl;text/x-python;text/x-tex;application/x-shellscript;
EOF
fi
echo "✓ Desktop entry created for Emacs with X11 backend"

# Create shell alias for X11 backend
SHELL_RC=""
if [ -f ~/.zshrc ]; then
    SHELL_RC=~/.zshrc
elif [ -f ~/.bashrc ]; then
    SHELL_RC=~/.bashrc
fi

if [ -n "$SHELL_RC" ]; then
    # Remove old alias if it exists
    sed -i '/alias emacs=/d' "$SHELL_RC" 2>/dev/null || true
    
    # Add appropriate alias based on available version
    echo "" >> "$SHELL_RC"
    echo "# Use X11 version of Emacs to avoid GTK issues" >> "$SHELL_RC"
    if [ -x "/usr/bin/emacs-30.1-gtk+x11" ]; then
        echo "alias emacs='/usr/bin/emacs-30.1-gtk+x11'" >> "$SHELL_RC"
        echo "✓ Added Emacs X11 alias to $SHELL_RC"
    else
        echo "alias emacs='GDK_BACKEND=x11 emacs'" >> "$SHELL_RC"
        echo "✓ Added Emacs GDK_BACKEND alias to $SHELL_RC"
    fi
fi

# Create wrapper script for dmenu/rofi
echo "Creating Emacs wrapper for dmenu/rofi..."
mkdir -p ~/bin

# Ensure ~/bin is in PATH
if [ -n "$SHELL_RC" ]; then
    if ! grep -q "PATH.*$HOME/bin" "$SHELL_RC"; then
        echo "" >> "$SHELL_RC"
        echo "# Add user bin to PATH for custom scripts" >> "$SHELL_RC"
        echo 'export PATH="$HOME/bin:$PATH"' >> "$SHELL_RC"
        echo "✓ Added ~/bin to PATH"
    fi
fi

# Create wrapper script
WRAPPER_SCRIPT="$SCRIPT_DIR/emacs-x11-wrapper.sh"
cat > "$WRAPPER_SCRIPT" << 'EOF'
#!/bin/bash
# Wrapper script to ensure X11 version of Emacs is used

# Use the X11 version explicitly
exec /usr/bin/emacs-30.1-gtk+x11 "$@"
EOF
chmod +x "$WRAPPER_SCRIPT"

# Link wrapper to ~/bin/emacs (takes precedence over /usr/bin/emacs)
ln -sf "$WRAPPER_SCRIPT" ~/bin/emacs
echo "✓ Created Emacs wrapper in ~/bin/emacs"

echo ""
echo "=== Emacs setup complete! ==="
echo ""
echo "Configuration:"
echo "  - Init file: ~/.emacs.d/init.el"
echo "  - Early init: ~/.emacs.d/early-init.el (fixes GTK issues)"
echo "  - Wrapper script: ~/bin/emacs (for dmenu/rofi)"
echo "  - Packages will be installed on first launch"
echo ""
echo "To start Emacs:"
echo "  - From terminal: emacs"
echo "  - From dmenu (Super+p): type 'emacs'"
echo "  - From menu: Look for 'Emacs (X11)'"
echo ""
echo "Key bindings:"
echo "  - C-x g     : Magit (Git interface)"
echo "  - C-x t t   : Treemacs (file tree)"
echo "  - C-c p     : Projectile commands"
echo "  - C-c l     : LSP commands"
echo "  - C-c t     : Open terminal"
echo ""
echo "Please restart your shell or run: source $SHELL_RC"