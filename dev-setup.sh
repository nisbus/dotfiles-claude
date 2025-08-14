#!/bin/bash

# Development Environment Setup Script
# Installs oh-my-zsh, development tools for Go, Erlang, and JavaScript/Node.js

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source utility functions
source "$SCRIPT_DIR/utils.sh"

echo -e "${GREEN}Setting up development environment...${NC}"

# Detect OS and package manager
detect_os
echo -e "${GREEN}Detected OS: $OS (Family: $OS_FAMILY)${NC}"
echo -e "${GREEN}Package Manager: $PKG_MANAGER${NC}"

# Function to check if a command exists
command_exists() {
    command -v "$1" &> /dev/null
}

# Install Oh My Zsh
install_oh_my_zsh() {
    echo -e "\n${GREEN}Installing Oh My Zsh...${NC}"
    
    if [ -d "$HOME/.oh-my-zsh" ]; then
        echo -e "${YELLOW}Oh My Zsh is already installed${NC}"
    else
        # Install dependencies first
        install_packages zsh git curl wget
        
        # Install Oh My Zsh
        sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
        
        echo -e "${GREEN}Oh My Zsh installed successfully${NC}"
    fi
    
    # Install popular Oh My Zsh plugins
    echo -e "${GREEN}Installing Oh My Zsh plugins...${NC}"
    
    # zsh-autosuggestions
    if [ ! -d "${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins/zsh-autosuggestions" ]; then
        git clone https://github.com/zsh-users/zsh-autosuggestions \
            ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
    fi
    
    # zsh-syntax-highlighting
    if [ ! -d "${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting" ]; then
        git clone https://github.com/zsh-users/zsh-syntax-highlighting \
            ${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
    fi
}

# Install Go
install_golang() {
    echo -e "\n${GREEN}Installing Go...${NC}"
    
    case "$PKG_MANAGER" in
        apt)
            # Remove old Go installation if present
            sudo apt-get remove -y golang-go 2>/dev/null || true
            
            # Install latest Go from official repo
            GO_VERSION="1.22.0"
            wget -q -O /tmp/go.tar.gz "https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz"
            sudo rm -rf /usr/local/go
            sudo tar -C /usr/local -xzf /tmp/go.tar.gz
            rm /tmp/go.tar.gz
            ;;
        dnf|yum)
            install_packages golang golang-x-tools-gopls
            ;;
        pacman)
            install_packages go gopls
            ;;
        zypper)
            install_packages go go-tools
            ;;
        brew)
            brew install go gopls
            ;;
        *)
            echo -e "${YELLOW}Please install Go manually for your system${NC}"
            ;;
    esac
    
    # Install Go development tools
    if command_exists go; then
        echo -e "${GREEN}Installing Go development tools...${NC}"
        export PATH=$PATH:/usr/local/go/bin:$HOME/go/bin
        
        # Install essential Go tools
        go install golang.org/x/tools/gopls@latest 2>/dev/null || true
        go install github.com/go-delve/delve/cmd/dlv@latest 2>/dev/null || true
        go install golang.org/x/tools/cmd/goimports@latest 2>/dev/null || true
        go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest 2>/dev/null || true
        go install github.com/fatih/gomodifytags@latest 2>/dev/null || true
        go install github.com/josharian/impl@latest 2>/dev/null || true
        
        echo -e "${GREEN}Go and tools installed successfully${NC}"
    fi
}

# Install Erlang/OTP
install_erlang() {
    echo -e "\n${GREEN}Installing Erlang/OTP...${NC}"
    
    case "$PKG_MANAGER" in
        apt)
            # Add Erlang Solutions repository for latest version
            wget -O- https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc | sudo apt-key add - 2>/dev/null || true
            echo "deb https://packages.erlang-solutions.com/ubuntu $(lsb_release -cs 2>/dev/null || echo "focal") contrib" | \
                sudo tee /etc/apt/sources.list.d/erlang-solutions.list > /dev/null
            sudo apt-get update
            install_packages esl-erlang elixir
            ;;
        dnf)
            install_packages erlang erlang-dialyzer erlang-doc elixir
            ;;
        yum)
            install_packages erlang elixir
            ;;
        pacman)
            install_packages erlang elixir
            ;;
        zypper)
            install_packages erlang elixir
            ;;
        brew)
            brew install erlang elixir
            ;;
        *)
            echo -e "${YELLOW}Please install Erlang manually for your system${NC}"
            ;;
    esac
    
    # Install rebar3 (Erlang build tool)
    if ! command_exists rebar3; then
        echo -e "${GREEN}Installing rebar3...${NC}"
        wget -q -O /tmp/rebar3 https://s3.amazonaws.com/rebar3/rebar3
        chmod +x /tmp/rebar3
        sudo mv /tmp/rebar3 /usr/local/bin/rebar3
    fi
    
    # Install erlang_ls (Erlang Language Server)
    if ! command_exists erlang_ls; then
        echo -e "${GREEN}Installing erlang_ls (Erlang Language Server)...${NC}"
        # Clone and build erlang_ls
        git clone https://github.com/erlang-ls/erlang_ls.git /tmp/erlang_ls 2>/dev/null || true
        cd /tmp/erlang_ls
        make
        sudo make install PREFIX=/usr/local
        cd -
        rm -rf /tmp/erlang_ls
    fi
    
    echo -e "${GREEN}Erlang/OTP installed successfully${NC}"
}

# Install Node.js and npm for JavaScript/Next.js development
install_nodejs() {
    echo -e "\n${GREEN}Installing Node.js and npm...${NC}"
    
    # Check if Node.js is already installed via Claude installation
    if command_exists node && command_exists npm; then
        echo -e "${YELLOW}Node.js is already installed${NC}"
        NODE_VERSION=$(node --version)
        NPM_VERSION=$(npm --version)
        echo -e "${GREEN}Node.js version: $NODE_VERSION${NC}"
        echo -e "${GREEN}npm version: $NPM_VERSION${NC}"
    else
        case "$PKG_MANAGER" in
            apt)
                curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -
                install_packages nodejs
                ;;
            dnf|yum)
                curl -fsSL https://rpm.nodesource.com/setup_lts.x | sudo bash -
                install_packages nodejs
                ;;
            pacman)
                install_packages nodejs npm
                ;;
            zypper)
                install_packages nodejs npm
                ;;
            brew)
                brew install node
                ;;
            *)
                echo -e "${YELLOW}Please install Node.js manually for your system${NC}"
                ;;
        esac
    fi
    
    # Setup npm for user-level global packages
    if command_exists npm; then
        echo -e "${GREEN}Configuring npm for user-level global packages...${NC}"
        mkdir -p ~/.npm-global
        npm config set prefix '~/.npm-global'
        
        # Install essential JavaScript/Next.js development tools
        echo -e "${GREEN}Installing JavaScript development tools...${NC}"
        export PATH=~/.npm-global/bin:$PATH
        
        npm install -g typescript 2>/dev/null || true
        npm install -g eslint 2>/dev/null || true
        npm install -g prettier 2>/dev/null || true
        npm install -g create-next-app 2>/dev/null || true
        npm install -g vercel 2>/dev/null || true
        npm install -g nodemon 2>/dev/null || true
        npm install -g ts-node 2>/dev/null || true
        
        echo -e "${GREEN}Node.js and JavaScript tools installed successfully${NC}"
    fi
}

# Install Docker properly
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
            *)
                echo -e "${YELLOW}Please install Docker manually for your system${NC}"
                return
                ;;
        esac
    fi
    
    # Start and enable Docker service
    if [ "$PKG_MANAGER" != "brew" ]; then
        sudo systemctl start docker 2>/dev/null || true
        sudo systemctl enable docker 2>/dev/null || true
        
        # Add user to docker group
        if ! groups $USER | grep -q docker; then
            echo -e "${GREEN}Adding $USER to docker group...${NC}"
            sudo usermod -aG docker $USER
            echo -e "${YELLOW}Note: You need to log out and back in for docker group changes to take effect${NC}"
        fi
    fi
    
    echo -e "${GREEN}Docker installation complete${NC}"
}

# Install additional development tools
install_dev_tools() {
    echo -e "\n${GREEN}Installing additional development tools...${NC}"
    
    # Install Make explicitly first (it's essential)
    echo -e "${GREEN}Installing Make...${NC}"
    install_packages make
    
    # Common development tools
    TOOLS=(
        emacs
        git
        curl
        wget
        gcc
        g++
        build-essential
        jq
        ripgrep
        fd-find
        bat
        htop
        tmux
        neovim
        tree
        ncdu
    )
    
    # Map package names for different distributions
    for tool in "${TOOLS[@]}"; do
        case "$tool" in
            "build-essential")
                echo -e "${GREEN}Installing build essentials (gcc, g++, make, etc.)...${NC}"
                if [ "$PKG_MANAGER" = "dnf" ] || [ "$PKG_MANAGER" = "yum" ]; then
                    install_packages gcc gcc-c++ make automake autoconf kernel-devel
                elif [ "$PKG_MANAGER" = "pacman" ]; then
                    install_packages base-devel
                elif [ "$PKG_MANAGER" = "zypper" ]; then
                    install_packages -t pattern devel_basis
                else
                    install_packages build-essential
                fi
                ;;
            "g++")
                if [ "$PKG_MANAGER" = "dnf" ] || [ "$PKG_MANAGER" = "yum" ]; then
                    install_packages gcc-c++
                else
                    install_packages g++
                fi
                ;;
            "fd-find")
                if [ "$PKG_MANAGER" = "dnf" ] || [ "$PKG_MANAGER" = "yum" ]; then
                    install_packages fd-find
                elif [ "$PKG_MANAGER" = "apt" ]; then
                    install_packages fd-find
                else
                    install_packages fd
                fi
                ;;
            "neovim")
                if [ "$PKG_MANAGER" = "apt" ]; then
                    install_packages neovim
                elif [ "$PKG_MANAGER" = "dnf" ]; then
                    install_packages neovim
                else
                    install_packages neovim || install_packages nvim
                fi
                ;;
            *)
                install_packages "$tool" 2>/dev/null || echo -e "${YELLOW}Warning: Could not install $tool${NC}"
                ;;
        esac
    done
}

# Main installation flow
main() {
    echo -e "${GREEN}=== Development Environment Setup ===${NC}"
    echo -e "${GREEN}This will install:${NC}"
    echo "  • Oh My Zsh with plugins"
    echo "  • Make and build essentials"
    echo "  • Docker and Docker Compose"
    echo "  • Go and development tools"
    echo "  • Erlang/OTP and rebar3"
    echo "  • Node.js, npm, and JavaScript tools"
    echo "  • Additional development utilities"
    echo ""
    
    # Install Oh My Zsh
    install_oh_my_zsh
    
    # Install development tools first (includes Make)
    install_dev_tools
    
    # Install Docker
    install_docker
    
    # Install development languages
    install_golang
    install_erlang
    install_nodejs
    
    # Update shell configuration
    echo -e "\n${GREEN}Updating shell configuration...${NC}"
    
    # Backup existing zshrc if it exists and isn't already linked
    if [ -f "$HOME/.zshrc" ] && [ ! -L "$HOME/.zshrc" ]; then
        backup_file "$HOME/.zshrc"
    fi
    
    # Link our zshrc
    if [ -f "$SCRIPT_DIR/zshrc" ]; then
        create_symlink "$SCRIPT_DIR/zshrc" "$HOME/.zshrc"
    elif [ -f "$SCRIPT_DIR/zsh/.zshrc" ]; then
        create_symlink "$SCRIPT_DIR/zsh/.zshrc" "$HOME/.zshrc"
    fi
    
    echo -e "\n${GREEN}=== Development Environment Setup Complete ===${NC}"
    echo -e "${YELLOW}Note:${NC}"
    echo "  1. Restart your shell or run: source ~/.zshrc"
    echo "  2. You may need to log out and back in for some changes"
    echo "  3. Docker may require adding your user to the docker group:"
    echo "     sudo usermod -aG docker $USER"
    echo ""
    echo -e "${GREEN}Installed tools:${NC}"
    command_exists go && echo "  ✓ Go $(go version 2>/dev/null | cut -d' ' -f3)"
    command_exists erl && echo "  ✓ Erlang $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell 2>/dev/null)"
    command_exists node && echo "  ✓ Node.js $(node --version)"
    command_exists npm && echo "  ✓ npm $(npm --version)"
    command_exists rebar3 && echo "  ✓ rebar3"
    [ -d "$HOME/.oh-my-zsh" ] && echo "  ✓ Oh My Zsh"
}

# Run main installation
main