#!/bin/bash

# Script to configure passwordless sudo for the current user
# This should be run with sudo once to set up the configuration

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

USER_NAME=$(whoami)

echo -e "${GREEN}Setting up passwordless sudo for user: $USER_NAME${NC}"

# Check if running with sudo
if [ "$EUID" -ne 0 ]; then 
    echo -e "${RED}This script must be run with sudo${NC}"
    echo -e "${YELLOW}Usage: sudo ./setup-sudo.sh${NC}"
    exit 1
fi

# Get the actual user who invoked sudo
if [ -n "$SUDO_USER" ]; then
    USER_NAME="$SUDO_USER"
fi

echo -e "${GREEN}Configuring sudo for user: $USER_NAME${NC}"

# Create sudoers.d directory if it doesn't exist
if [ ! -d /etc/sudoers.d ]; then
    mkdir -p /etc/sudoers.d
    chmod 750 /etc/sudoers.d
fi

# Create a sudoers file for the user
SUDOERS_FILE="/etc/sudoers.d/99-${USER_NAME}-nopasswd"

echo -e "${GREEN}Creating sudoers configuration at: $SUDOERS_FILE${NC}"

# Write the configuration
cat > "$SUDOERS_FILE" << EOF
# Allow $USER_NAME to run any command without password
$USER_NAME ALL=(ALL) NOPASSWD: ALL

# Alternative: Allow wheel group to run without password
# %wheel ALL=(ALL) NOPASSWD: ALL
EOF

# Set proper permissions (must be 0440 for sudoers files)
chmod 0440 "$SUDOERS_FILE"

# Validate the sudoers file
echo -e "${GREEN}Validating sudoers configuration...${NC}"
if visudo -c -f "$SUDOERS_FILE"; then
    echo -e "${GREEN}✓ Sudoers configuration is valid${NC}"
else
    echo -e "${RED}✗ Sudoers configuration is invalid, removing it${NC}"
    rm -f "$SUDOERS_FILE"
    exit 1
fi

# Check if user is in wheel group
if groups "$USER_NAME" | grep -q wheel; then
    echo -e "${GREEN}✓ User $USER_NAME is already in wheel group${NC}"
else
    echo -e "${YELLOW}Adding user $USER_NAME to wheel group...${NC}"
    usermod -aG wheel "$USER_NAME"
    echo -e "${GREEN}✓ User added to wheel group${NC}"
fi

echo -e "\n${GREEN}=== Setup Complete ===${NC}"
echo -e "${GREEN}User $USER_NAME can now use sudo without a password${NC}"
echo -e "${YELLOW}Note: You may need to log out and back in for group changes to take effect${NC}"

# Test the configuration
echo -e "\n${GREEN}Testing configuration...${NC}"
su - "$USER_NAME" -c "sudo -n true 2>/dev/null" && echo -e "${GREEN}✓ Passwordless sudo is working${NC}" || echo -e "${YELLOW}⚠ Please log out and back in to test passwordless sudo${NC}"