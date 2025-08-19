#!/bin/bash
# LSP Server Installation Script for Emacs Development Environment
# Supports: Ubuntu/Debian, Fedora/RHEL/CentOS, Arch Linux, openSUSE
# Author: Generated for dotfiles-claude emacs configuration

set -e  # Exit on any error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# Check if running as root
check_root() {
    if [[ $EUID -eq 0 ]]; then
        log_error "This script should not be run as root. Please run as a regular user."
        log_info "The script will use sudo when necessary."
        exit 1
    fi
}

# Detect Linux distribution
detect_distro() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        DISTRO=$ID
        DISTRO_VERSION=$VERSION_ID
    elif [ -f /etc/redhat-release ]; then
        DISTRO="centos"
    elif [ -f /etc/debian_version ]; then
        DISTRO="debian"
    else
        log_error "Unable to detect Linux distribution"
        exit 1
    fi
    
    log_info "Detected distribution: $DISTRO $DISTRO_VERSION"
}

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Install system packages based on distribution
install_system_packages() {
    log_info "Installing system packages..."
    
    case "$DISTRO" in
        ubuntu|debian)
            log_info "Installing packages for Ubuntu/Debian..."
            sudo apt update
            sudo apt install -y curl wget git build-essential software-properties-common
            
            # Install Node.js from NodeSource (for latest version)
            if ! command_exists node; then
                log_info "Installing Node.js from NodeSource..."
                curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -
                sudo apt install -y nodejs
            fi
            
            # Install Go
            if ! command_exists go; then
                log_info "Installing Go..."
                sudo apt install -y golang-go
            fi
            
            # Install Python tools
            sudo apt install -y python3 python3-pip python3-venv python3-dev
            
            # Install Erlang/Elixir (optional)
            if [ "$INSTALL_ERLANG" = "yes" ]; then
                sudo apt install -y erlang elixir
            fi
            ;;
            
        fedora|rhel|centos|rocky|almalinux)
            log_info "Installing packages for Fedora/RHEL/CentOS..."
            sudo dnf update -y
            sudo dnf install -y curl wget git gcc make dnf-plugins-core
            sudo dnf groupinstall -y "Development Tools"
            
            # Install Node.js
            if ! command_exists node; then
                log_info "Installing Node.js..."
                sudo dnf install -y nodejs npm
            fi
            
            # Install Go
            if ! command_exists go; then
                log_info "Installing Go..."
                sudo dnf install -y golang
            fi
            
            # Install Python tools
            sudo dnf install -y python3 python3-pip python3-devel
            
            # Install Erlang/Elixir (optional)
            if [ "$INSTALL_ERLANG" = "yes" ]; then
                sudo dnf install -y erlang elixir
            fi
            ;;
            
        arch|manjaro)
            log_info "Installing packages for Arch Linux..."
            sudo pacman -Syu --noconfirm
            sudo pacman -S --noconfirm base-devel git curl wget
            
            # Install programming languages
            if ! command_exists node; then
                sudo pacman -S --noconfirm nodejs npm
            fi
            if ! command_exists go; then
                sudo pacman -S --noconfirm go
            fi
            sudo pacman -S --noconfirm python python-pip python-virtualenv
            
            # Install Erlang/Elixir (optional)
            if [ "$INSTALL_ERLANG" = "yes" ]; then
                sudo pacman -S --noconfirm erlang elixir
            fi
            ;;
            
        opensuse*|sles)
            log_info "Installing packages for openSUSE..."
            sudo zypper refresh
            sudo zypper update -y
            sudo zypper install -y git curl wget gcc make
            sudo zypper install -y -t pattern devel_basis
            
            # Install programming languages
            if ! command_exists node; then
                sudo zypper install -y nodejs npm
            fi
            if ! command_exists go; then
                sudo zypper install -y go
            fi
            sudo zypper install -y python3 python3-pip python3-devel
            
            # Install Erlang/Elixir (optional)
            if [ "$INSTALL_ERLANG" = "yes" ]; then
                sudo zypper install -y erlang
            fi
            ;;
            
        *)
            log_error "Unsupported distribution: $DISTRO"
            log_info "Supported distributions: Ubuntu, Debian, Fedora, RHEL, CentOS, Arch Linux, openSUSE"
            exit 1
            ;;
    esac
    
    log_success "System packages installed successfully"
}

# Set up environment paths
setup_environment() {
    log_info "Setting up environment paths..."
    
    # Go PATH setup
    if command_exists go; then
        GOPATH=$(go env GOPATH 2>/dev/null || echo "$HOME/go")
        if [[ ":$PATH:" != *":$GOPATH/bin:"* ]]; then
            log_info "Adding Go bin directory to PATH..."
            echo "export PATH=\$PATH:\$(go env GOPATH)/bin" >> ~/.bashrc
            export PATH=$PATH:$GOPATH/bin
        fi
    fi
    
    # npm global PATH setup (to avoid sudo npm install -g)
    if command_exists npm; then
        NPM_GLOBAL_DIR="$HOME/.npm-global"
        if [ ! -d "$NPM_GLOBAL_DIR" ]; then
            log_info "Setting up npm global directory..."
            mkdir -p "$NPM_GLOBAL_DIR"
            npm config set prefix "$NPM_GLOBAL_DIR"
            echo "export PATH=$NPM_GLOBAL_DIR/bin:\$PATH" >> ~/.bashrc
            export PATH=$NPM_GLOBAL_DIR/bin:$PATH
        fi
    fi
    
    log_success "Environment paths configured"
}

# Install Go LSP server
install_go_lsp() {
    if ! command_exists go; then
        log_warning "Go not found, skipping gopls installation"
        return
    fi
    
    log_info "Installing Go LSP server (gopls)..."
    
    if command_exists gopls; then
        log_info "gopls already installed, updating..."
    fi
    
    go install golang.org/x/tools/gopls@latest
    
    if command_exists gopls; then
        log_success "gopls installed successfully: $(gopls version)"
    else
        log_error "Failed to install gopls"
        return 1
    fi
}

# Install Python LSP server
install_python_lsp() {
    if ! command_exists npm; then
        log_warning "npm not found, skipping Python LSP server installation"
        return
    fi
    
    log_info "Installing Python LSP server (Pyright)..."
    
    npm install -g pyright
    
    if command_exists pyright; then
        log_success "Pyright installed successfully: $(pyright --version)"
    else
        log_error "Failed to install Pyright"
        return 1
    fi
    
}

# Install JavaScript/TypeScript LSP server
install_js_ts_lsp() {
    if ! command_exists npm; then
        log_warning "npm not found, skipping JavaScript/TypeScript LSP server installation"
        return
    fi
    
    log_info "Installing JavaScript/TypeScript LSP servers..."
    
    npm install -g typescript-language-server typescript prettier eslint
    
    # Verify installations
    local success=true
    if command_exists typescript-language-server; then
        log_success "typescript-language-server installed: $(typescript-language-server --version)"
    else
        log_error "Failed to install typescript-language-server"
        success=false
    fi
    
    if command_exists tsc; then
        log_success "TypeScript compiler installed: $(tsc --version)"
    else
        log_error "Failed to install TypeScript compiler"
        success=false
    fi
    
    if command_exists prettier; then
        log_success "Prettier installed: $(prettier --version)"
    else
        log_error "Failed to install Prettier"
        success=false
    fi
    
    if [ "$success" = true ]; then
        log_success "JavaScript/TypeScript LSP servers installed successfully"
    else
        return 1
    fi
}

# Install Erlang LSP server
install_erlang_lsp() {
    if [ "$INSTALL_ERLANG" != "yes" ]; then
        log_info "Erlang LSP installation skipped (optional)"
        return
    fi
    
    if ! command_exists erl; then
        log_warning "Erlang not found, skipping erlang_ls installation"
        return
    fi
    
    log_info "Installing Erlang LSP server (erlang_ls)..."
    
    # Try package manager first
    case "$DISTRO" in
        ubuntu|debian)
            if sudo apt install -y erlang-ls 2>/dev/null; then
                log_success "erlang_ls installed via package manager"
                return
            fi
            ;;
        fedora|rhel|centos|rocky|almalinux)
            if sudo dnf install -y erlang-ls 2>/dev/null; then
                log_success "erlang_ls installed via package manager"
                return
            fi
            ;;
        arch|manjaro)
            if command_exists yay; then
                if yay -S --noconfirm erlang-ls-git 2>/dev/null; then
                    log_success "erlang_ls installed via AUR"
                    return
                fi
            fi
            ;;
    esac
    
    # Fallback to manual installation
    log_info "Package manager installation failed, trying manual installation..."
    
    # Try downloading pre-built binary
    local erlang_ls_url="https://github.com/erlang-ls/erlang_ls/releases/latest/download/erlang_ls"
    local temp_dir=$(mktemp -d)
    
    if curl -L -o "$temp_dir/erlang_ls" "$erlang_ls_url" 2>/dev/null; then
        chmod +x "$temp_dir/erlang_ls"
        if "$temp_dir/erlang_ls" --help >/dev/null 2>&1; then
            sudo mv "$temp_dir/erlang_ls" /usr/local/bin/
            log_success "erlang_ls installed from pre-built binary"
            return
        fi
    fi
    
    log_warning "Failed to install erlang_ls automatically"
    log_info "You may need to build it from source: https://github.com/erlang-ls/erlang_ls"
    
    rm -rf "$temp_dir"
}

# Install Elixir LSP server
install_elixir_lsp() {
    if [ "$INSTALL_ERLANG" != "yes" ]; then
        log_info "Elixir LSP installation skipped (optional)"
        return
    fi
    
    if ! command_exists elixir; then
        log_warning "Elixir not found, skipping elixir-ls installation"
        return
    fi
    
    log_info "Installing Elixir LSP server (elixir-ls)..."
    
    # Try package manager first
    case "$DISTRO" in
        arch|manjaro)
            if command_exists yay; then
                if yay -S --noconfirm elixir-ls 2>/dev/null; then
                    log_success "elixir-ls installed via AUR"
                    return
                fi
            fi
            ;;
    esac
    
    # Manual installation
    log_info "Building elixir-ls from source..."
    local temp_dir=$(mktemp -d)
    
    if git clone https://github.com/elixir-lsp/elixir-ls.git "$temp_dir/elixir-ls" 2>/dev/null; then
        cd "$temp_dir/elixir-ls"
        # Get version for VERSION file
        local version=$(git describe --tags --always)
        
        if mix deps.get && MIX_ENV=prod mix compile; then
            sudo mkdir -p /opt/elixir-ls
            sudo cp -r * /opt/elixir-ls/
            echo "$version" | sudo tee /opt/elixir-ls/scripts/VERSION > /dev/null
            sudo ln -sf /opt/elixir-ls/scripts/language_server.sh /usr/local/bin/elixir-ls
            log_success "elixir-ls built and installed successfully"
        else
            log_warning "Failed to build elixir-ls from source"
        fi
        cd - >/dev/null
    else
        log_warning "Failed to clone elixir-ls repository"
    fi
    
    rm -rf "$temp_dir"
}

# Verify all installations
verify_installations() {
    log_info "Verifying LSP server installations..."
    
    local all_good=true
    
    echo "=== LSP Server Verification ==="
    
    # Go (gopls)
    if command_exists gopls; then
        log_success "Go (gopls): $(gopls version)"
    else
        log_error "Go (gopls): Not found"
        all_good=false
    fi
    
    # Python (pyright)
    if command_exists pyright; then
        log_success "Python (pyright): $(pyright --version)"
    else
        log_error "Python (pyright): Not found"
        all_good=false
    fi
    
    # TypeScript
    if command_exists typescript-language-server; then
        log_success "TypeScript: $(typescript-language-server --version)"
    else
        log_error "TypeScript: Not found"
        all_good=false
    fi
    
    # Prettier
    if command_exists prettier; then
        log_success "Prettier: $(prettier --version)"
    else
        log_error "Prettier: Not found"
        all_good=false
    fi
    
    # Erlang (optional)
    if [ "$INSTALL_ERLANG" = "yes" ]; then
        if command_exists erlang_ls; then
            log_success "Erlang (erlang_ls): Found"
        else
            log_warning "Erlang (erlang_ls): Not found (optional)"
        fi
        
        if command_exists elixir-ls; then
            log_success "Elixir (elixir-ls): Found"
        else
            log_warning "Elixir (elixir-ls): Not found (optional)"
        fi
    fi
    
    echo ""
    echo "=== Environment Check ==="
    
    if command_exists go; then
        echo "Go path: $(go env GOPATH)"
        echo "Go version: $(go version)"
    fi
    
    if command_exists node; then
        echo "Node version: $(node --version)"
        echo "npm version: $(npm --version)"
    fi
    
    if command_exists python3; then
        echo "Python version: $(python3 --version)"
    fi
    
    if command_exists erl && [ "$INSTALL_ERLANG" = "yes" ]; then
        echo "Erlang version: $(erl -eval 'io:format("~s~n", [erlang:system_info(system_version)]), halt().' 2>/dev/null | head -1)"
    fi
    
    if command_exists elixir && [ "$INSTALL_ERLANG" = "yes" ]; then
        echo "Elixir version: $(elixir --version | head -1)"
    fi
    
    echo ""
    
    if [ "$all_good" = true ]; then
        log_success "All required LSP servers installed successfully!"
        log_info "You may need to restart your shell or run: source ~/.bashrc"
    else
        log_error "Some LSP servers failed to install. Please check the errors above."
        return 1
    fi
}

# Show post-installation instructions
show_post_install() {
    echo ""
    log_info "Post-installation steps:"
    echo ""
    echo "1. Restart your terminal or run: source ~/.bashrc"
    echo "2. Open Emacs and the LSP servers will be automatically used when you open supported files"
    echo "3. For Python projects, consider using virtual environments:"
    echo "   python3 -m venv venv && source venv/bin/activate"
    echo "4. For Go projects, ensure you have go.mod file in your project root"
    echo "5. For JavaScript/TypeScript projects, run 'npm install' in your project directory"
    echo ""
    echo "Troubleshooting:"
    echo "- If LSP servers don't work in Emacs, check: M-x my/check-lsp-servers"
    echo "- For detailed setup information, see: LSP_SETUP.md"
    echo "- Enable LSP logging in Emacs: (setq lsp-log-io t)"
    echo ""
}

# Print usage information
show_usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -h, --help      Show this help message"
    echo "  -e, --erlang    Include Erlang/Elixir LSP servers (optional)"
    echo "  -y, --yes       Answer yes to all prompts (non-interactive mode)"
    echo "  --dry-run       Show what would be installed without actually installing"
    echo ""
    echo "Examples:"
    echo "  $0                    # Install core LSP servers (Go, Python, JS/TS)"
    echo "  $0 --erlang          # Install all LSP servers including Erlang/Elixir"
    echo "  $0 --yes --erlang    # Non-interactive installation with all servers"
    echo ""
}

# Parse command line arguments
parse_args() {
    INSTALL_ERLANG="no"
    NON_INTERACTIVE="no"
    DRY_RUN="no"
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_usage
                exit 0
                ;;
            -e|--erlang)
                INSTALL_ERLANG="yes"
                shift
                ;;
            -y|--yes)
                NON_INTERACTIVE="yes"
                shift
                ;;
            --dry-run)
                DRY_RUN="yes"
                shift
                ;;
            *)
                log_error "Unknown option: $1"
                show_usage
                exit 1
                ;;
        esac
    done
}

# Main installation function
main() {
    echo "LSP Server Installation Script for Emacs"
    echo "======================================="
    echo ""
    
    parse_args "$@"
    
    if [ "$DRY_RUN" = "yes" ]; then
        log_info "DRY RUN MODE - No packages will be installed"
    fi
    
    check_root
    detect_distro
    
    if [ "$NON_INTERACTIVE" = "no" ]; then
        echo "This script will install LSP servers for:"
        echo "  ✓ Go (gopls)"
        echo "  ✓ Python (pyright)"
        echo "  ✓ JavaScript/TypeScript (typescript-language-server, prettier)"
        if [ "$INSTALL_ERLANG" = "yes" ]; then
            echo "  ✓ Erlang (erlang_ls) [optional]"
            echo "  ✓ Elixir (elixir-ls) [optional]"
        else
            echo "  - Erlang/Elixir (use --erlang flag to include)"
        fi
        echo ""
        read -p "Continue with installation? (y/N): " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            log_info "Installation cancelled"
            exit 0
        fi
    fi
    
    if [ "$DRY_RUN" = "yes" ]; then
        log_info "Would install system packages for: $DISTRO"
        log_info "Would set up environment paths"
        log_info "Would install Go LSP server (gopls)"
        log_info "Would install Python LSP server (pyright)"
        log_info "Would install JavaScript/TypeScript LSP servers"
        if [ "$INSTALL_ERLANG" = "yes" ]; then
            log_info "Would install Erlang LSP server (erlang_ls)"
            log_info "Would install Elixir LSP server (elixir-ls)"
        fi
        log_info "Dry run completed"
        exit 0
    fi
    
    # Perform installations
    install_system_packages
    setup_environment
    
    # Source bashrc to get updated PATH
    export PATH="$HOME/.npm-global/bin:$PATH:$(go env GOPATH 2>/dev/null || echo "$HOME/go")/bin"
    
    install_go_lsp
    install_python_lsp
    install_js_ts_lsp
    
    if [ "$INSTALL_ERLANG" = "yes" ]; then
        install_erlang_lsp
        install_elixir_lsp
    fi
    
    verify_installations
    show_post_install
    
    log_success "LSP server installation completed!"
}

# Run main function with all arguments
main "$@"