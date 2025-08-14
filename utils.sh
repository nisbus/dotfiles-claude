#!/bin/bash

# Utility functions for cross-platform compatibility

# Colors for output
export RED='\033[0;31m'
export GREEN='\033[0;32m'
export YELLOW='\033[1;33m'
export NC='\033[0m' # No Color

# Detect the operating system and package manager
detect_os() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        OS=$ID
        OS_FAMILY=$ID_LIKE
        OS_VERSION=$VERSION_ID
    elif [ -f /etc/debian_version ]; then
        OS="debian"
        OS_FAMILY="debian"
    elif [ -f /etc/redhat-release ]; then
        OS="rhel"
        OS_FAMILY="rhel fedora"
    elif [ "$(uname)" == "Darwin" ]; then
        OS="macos"
        OS_FAMILY="macos"
    else
        OS="unknown"
        OS_FAMILY="unknown"
    fi
    
    # Detect package manager
    if command -v apt-get &> /dev/null; then
        PKG_MANAGER="apt"
        PKG_UPDATE="sudo apt-get update"
        PKG_INSTALL="sudo apt-get install -y"
        PKG_SEARCH="apt-cache search"
    elif command -v dnf &> /dev/null; then
        PKG_MANAGER="dnf"
        PKG_UPDATE="sudo dnf check-update || true"
        PKG_INSTALL="sudo dnf install -y"
        PKG_SEARCH="dnf search"
    elif command -v yum &> /dev/null; then
        PKG_MANAGER="yum"
        PKG_UPDATE="sudo yum check-update || true"
        PKG_INSTALL="sudo yum install -y"
        PKG_SEARCH="yum search"
    elif command -v pacman &> /dev/null; then
        PKG_MANAGER="pacman"
        PKG_UPDATE="sudo pacman -Sy"
        PKG_INSTALL="sudo pacman -S --noconfirm"
        PKG_SEARCH="pacman -Ss"
    elif command -v zypper &> /dev/null; then
        PKG_MANAGER="zypper"
        PKG_UPDATE="sudo zypper refresh"
        PKG_INSTALL="sudo zypper install -y"
        PKG_SEARCH="zypper search"
    elif command -v brew &> /dev/null; then
        PKG_MANAGER="brew"
        PKG_UPDATE="brew update"
        PKG_INSTALL="brew install"
        PKG_SEARCH="brew search"
    else
        PKG_MANAGER="unknown"
        PKG_UPDATE=""
        PKG_INSTALL=""
        PKG_SEARCH=""
    fi
    
    export OS
    export OS_FAMILY
    export OS_VERSION
    export PKG_MANAGER
    export PKG_UPDATE
    export PKG_INSTALL
    export PKG_SEARCH
}

# Map package names between distributions
get_package_name() {
    local pkg="$1"
    
    case "$PKG_MANAGER" in
        apt)
            case "$pkg" in
                "fira-code-fonts") echo "fonts-firacode" ;;
                "jetbrains-mono-fonts") echo "fonts-jetbrains-mono" ;;
                "cascadia-code-fonts") echo "fonts-cascadia-code" ;;
                "hack-fonts") echo "fonts-hack" ;;
                "nodejs") echo "nodejs" ;;
                *) echo "$pkg" ;;
            esac
            ;;
        dnf|yum)
            case "$pkg" in
                "fonts-firacode") echo "fira-code-fonts" ;;
                "fonts-jetbrains-mono") echo "jetbrains-mono-fonts" ;;
                "fonts-cascadia-code") echo "cascadia-code-fonts" ;;
                "fonts-hack") echo "hack-fonts" ;;
                "xclip") echo "xclip" ;;
                "xdotool") echo "xdotool" ;;
                "alacritty") echo "alacritty" ;;
                "nodejs") echo "nodejs" ;;
                "barrier") echo "barrier" ;;
                *) echo "$pkg" ;;
            esac
            ;;
        *)
            echo "$pkg"
            ;;
    esac
}

# Install packages with proper name mapping
install_packages() {
    detect_os
    
    if [ "$PKG_MANAGER" = "unknown" ]; then
        echo -e "${RED}Unable to detect package manager. Please install packages manually.${NC}"
        return 1
    fi
    
    echo -e "${GREEN}Using package manager: $PKG_MANAGER${NC}"
    
    # Update package lists
    if [ -n "$PKG_UPDATE" ]; then
        echo -e "${GREEN}Updating package lists...${NC}"
        eval "$PKG_UPDATE"
    fi
    
    # Install each package with proper name mapping
    for pkg in "$@"; do
        local mapped_pkg=$(get_package_name "$pkg")
        echo -e "${GREEN}Installing $mapped_pkg...${NC}"
        eval "$PKG_INSTALL $mapped_pkg" || echo -e "${YELLOW}Warning: Failed to install $mapped_pkg${NC}"
    done
}

# Check if a command exists
command_exists() {
    command -v "$1" &> /dev/null
}

# Function to create backup of existing files
backup_file() {
    if [ -f "$1" ]; then
        echo -e "${YELLOW}Backing up existing $1 to $1.backup${NC}"
        cp "$1" "$1.backup"
    fi
}

# Function to create symlink
create_symlink() {
    local source="$1"
    local target="$2"
    
    if [ -e "$target" ] || [ -L "$target" ]; then
        backup_file "$target"
        rm -f "$target"
    fi
    
    echo -e "${GREEN}Creating symlink: $target -> $source${NC}"
    ln -s "$source" "$target"
}

# Initialize OS detection when sourced
detect_os