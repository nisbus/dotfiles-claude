#!/bin/bash
# Wrapper script to ensure X11 version of Emacs is used

# Use the X11 version explicitly
exec /usr/bin/emacs-30.1-gtk+x11 "$@"
