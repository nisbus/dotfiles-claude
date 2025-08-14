#!/bin/bash

# Docker Installation Script with proper group permissions
# This script installs Docker and ensures the user is added to the docker group

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PARENT_DIR="$(dirname "$SCRIPT_DIR")"

# Source utility functions
source "$PARENT_DIR/utils.sh"

echo -e "${GREEN}Docker Installation and Setup${NC}"

# Detect OS and package manager
detect_os
echo -e "${GREEN}Detected OS: $OS (Family: $OS_FAMILY)${NC}"
echo -e "${GREEN}Package Manager: $PKG_MANAGER${NC}"

# Function to check if a command exists
command_exists() {
    command -v "$1" &> /dev/null
}

# Install Docker
install_docker() {
    echo -e "\n${GREEN}Installing Docker...${NC}"
    
    if command_exists docker; then
        echo -e "${YELLOW}Docker is already installed${NC}"
        DOCKER_VERSION=$(docker --version 2>/dev/null || echo "unknown")
        echo -e "${GREEN}Docker version: $DOCKER_VERSION${NC}"
    else
        case "$PKG_MANAGER" in
            dnf)
                # Fedora
                sudo dnf remove -y docker docker-client docker-client-latest docker-common \
                    docker-latest docker-latest-logrotate docker-logrotate docker-selinux \
                    docker-engine-selinux docker-engine 2>/dev/null || true
                
                install_packages dnf-plugins-core
                
                # For Fedora 42+, use the new syntax
                if [ -f /etc/fedora-release ]; then
                    FEDORA_VERSION=$(rpm -E %fedora)
                    if [ "$FEDORA_VERSION" -ge 42 ]; then
                        # New syntax for Fedora 42+
                        sudo dnf config-manager addrepo --from-repofile=https://download.docker.com/linux/fedora/docker-ce.repo
                    else
                        # Old syntax for older Fedora versions
                        sudo dnf config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo
                    fi
                else
                    # Default to old syntax if we can't determine version
                    sudo dnf config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo
                fi
                
                install_packages docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
                ;;
            apt)
                # Ubuntu/Debian
                sudo apt-get remove -y docker docker-engine docker.io containerd runc 2>/dev/null || true
                
                install_packages ca-certificates gnupg lsb-release
                
                # Add Docker's official GPG key
                sudo mkdir -p /etc/apt/keyrings
                curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
                
                # Set up repository
                echo \
                  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
                  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
                
                sudo apt-get update
                install_packages docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
                ;;
            yum)
                # RHEL/CentOS
                sudo yum remove -y docker docker-client docker-client-latest docker-common \
                    docker-latest docker-latest-logrotate docker-logrotate docker-engine 2>/dev/null || true
                
                install_packages yum-utils
                sudo yum-config-manager --add-repo https://download.docker.com/linux/centos/docker-ce.repo
                install_packages docker-ce docker-ce-cli containerd.io docker-compose-plugin
                ;;
            pacman)
                # Arch
                install_packages docker docker-compose
                ;;
            zypper)
                # openSUSE
                install_packages docker docker-compose
                ;;
            brew)
                # macOS
                brew install --cask docker
                echo -e "${YELLOW}Docker Desktop installed. Please start Docker Desktop from Applications.${NC}"
                return
                ;;
            *)
                echo -e "${RED}Unsupported package manager. Please install Docker manually.${NC}"
                echo -e "Visit: https://docs.docker.com/engine/install/"
                return 1
                ;;
        esac
    fi
    
    # Start and enable Docker service (Linux only)
    if [ "$PKG_MANAGER" != "brew" ]; then
        echo -e "${GREEN}Starting Docker service...${NC}"
        sudo systemctl start docker 2>/dev/null || true
        sudo systemctl enable docker 2>/dev/null || true
    fi
    
    echo -e "${GREEN}Docker installation complete${NC}"
}

# Configure Docker permissions
configure_docker_permissions() {
    echo -e "\n${GREEN}Configuring Docker permissions...${NC}"
    
    # Only proceed on Linux
    if [ "$PKG_MANAGER" = "brew" ]; then
        echo -e "${YELLOW}Docker Desktop on macOS handles permissions automatically.${NC}"
        return
    fi
    
    # Check if docker group exists, create if it doesn't
    if ! getent group docker > /dev/null 2>&1; then
        echo -e "${GREEN}Creating docker group...${NC}"
        sudo groupadd docker
    fi
    
    # Add current user to docker group
    if ! groups $USER | grep -q docker; then
        echo -e "${GREEN}Adding $USER to docker group...${NC}"
        sudo usermod -aG docker $USER
        echo -e "${YELLOW}IMPORTANT: You need to log out and back in for group changes to take effect!${NC}"
        echo -e "${YELLOW}Or run: newgrp docker (for current session only)${NC}"
    else
        echo -e "${GREEN}User $USER is already in docker group${NC}"
    fi
    
    # Fix docker socket permissions if needed
    if [ -e /var/run/docker.sock ]; then
        echo -e "${GREEN}Setting correct permissions on Docker socket...${NC}"
        sudo chown root:docker /var/run/docker.sock 2>/dev/null || true
        sudo chmod 660 /var/run/docker.sock 2>/dev/null || true
    fi
}

# Test Docker installation
test_docker() {
    echo -e "\n${GREEN}Testing Docker installation...${NC}"
    
    # Try without sudo first
    if docker run hello-world 2>/dev/null; then
        echo -e "${GREEN}✓ Docker is working correctly without sudo!${NC}"
    else
        # Try with sudo
        if sudo docker run hello-world 2>/dev/null; then
            echo -e "${YELLOW}Docker works with sudo. You need to log out and back in for group changes.${NC}"
        else
            echo -e "${RED}Docker test failed. Please check the installation.${NC}"
            return 1
        fi
    fi
}

# Main execution
main() {
    echo -e "${GREEN}=== Docker Installation and Configuration ===${NC}"
    
    # Install Docker
    install_docker
    
    # Configure permissions
    configure_docker_permissions
    
    # Test installation
    test_docker
    
    echo -e "\n${GREEN}=== Docker Setup Complete ===${NC}"
    
    # Check if user needs to log out
    if ! groups $USER | grep -q docker; then
        echo -e "\n${YELLOW}ACTION REQUIRED:${NC}"
        echo -e "You've been added to the docker group, but need to:"
        echo -e "  1. Log out and log back in"
        echo -e "     OR"
        echo -e "  2. Run: newgrp docker (temporary fix for current session)"
        echo -e "\nAfter that, you can run Docker commands without sudo!"
    else
        echo -e "\n${GREEN}Docker is ready to use!${NC}"
        echo -e "Try: docker run hello-world"
    fi
}

# Run main function
main