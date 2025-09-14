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
                "xrandr") echo "x11-xserver-utils" ;;
                "build-essential") echo "build-essential" ;;
                "gcc-c++") echo "g++" ;;
                "fd-find") echo "fd-find" ;;
                "ripgrep") echo "ripgrep" ;;
                "bat") echo "bat" ;;
                "neovim") echo "neovim" ;;
                "ncdu") echo "ncdu" ;;
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

# Check if a package is already installed
is_package_installed() {
    local pkg="$1"
    local mapped_pkg=$(get_package_name "$pkg")

    case "$PKG_MANAGER" in
        apt)
            dpkg -l "$mapped_pkg" 2>/dev/null | grep -q '^ii'
            ;;
        dnf|yum)
            rpm -q "$mapped_pkg" &>/dev/null
            ;;
        pacman)
            pacman -Qi "$mapped_pkg" &>/dev/null
            ;;
        *)
            return 1
            ;;
    esac
}

# Install packages with proper name mapping and optimization
install_packages() {
    detect_os

    if [ "$PKG_MANAGER" = "unknown" ]; then
        echo -e "${RED}Unable to detect package manager. Please install packages manually.${NC}"
        return 1
    fi

    echo -e "${GREEN}Using package manager: $PKG_MANAGER${NC}"

    # Check which packages actually need installation
    local packages_to_install=()
    local already_installed=()

    for pkg in "$@"; do
        local mapped_pkg=$(get_package_name "$pkg")
        if is_package_installed "$pkg"; then
            already_installed+=("$mapped_pkg")
        else
            packages_to_install+=("$mapped_pkg")
        fi
    done

    # Report already installed packages
    if [ ${#already_installed[@]} -gt 0 ]; then
        echo -e "${YELLOW}Already installed: ${already_installed[*]}${NC}"
    fi

    # Only proceed with installation if there are packages to install
    if [ ${#packages_to_install[@]} -eq 0 ]; then
        echo -e "${GREEN}All requested packages are already installed${NC}"
        return 0
    fi

    # Update package lists only once, only if we have packages to install
    if [ -n "$PKG_UPDATE" ]; then
        echo -e "${GREEN}Updating package lists...${NC}"
        eval "$PKG_UPDATE"
    fi

    # Install packages that need installation
    echo -e "${GREEN}Installing: ${packages_to_install[*]}${NC}"
    eval "$PKG_INSTALL ${packages_to_install[*]}" || echo -e "${YELLOW}Warning: Some packages failed to install${NC}"
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