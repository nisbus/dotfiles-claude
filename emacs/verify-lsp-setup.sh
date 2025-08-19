#!/bin/bash
# LSP Server Verification Script
# Checks if all LSP servers are properly installed and accessible

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[✓]${NC} $1"; }
log_warning() { echo -e "${YELLOW}[⚠]${NC} $1"; }
log_error() { echo -e "${RED}[✗]${NC} $1"; }

# Check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Test LSP server by attempting to get version/help
test_lsp_server() {
    local server_name=$1
    local command=$2
    local test_args=$3
    
    if command_exists "$command"; then
        # Special handling for different servers
        case "$command" in
            "erlang_ls")
                # erlang_ls outputs help to stderr, so check both stdout and stderr
                if timeout 5 "$command" --version >/dev/null 2>&1; then
                    return 0
                else
                    return 2
                fi
                ;;
            "elixir-ls")
                # elixir-ls might be a script, test if it exists and is executable
                if [ -x "$(command -v "$command")" ]; then
                    return 0
                else
                    return 2
                fi
                ;;
            *)
                if timeout 5 "$command" $test_args >/dev/null 2>&1; then
                    return 0
                else
                    return 2  # Command exists but doesn't respond properly
                fi
                ;;
        esac
    else
        return 1  # Command not found
    fi
}

echo "LSP Server Verification Script"
echo "=============================="
echo ""

# Track overall success
overall_success=true
required_failures=0
optional_failures=0

echo "=== Core Language Support ==="

# Go (gopls)
echo -n "Go (gopls): "
case $(test_lsp_server "gopls" "gopls" "version"; echo $?) in
    0)
        version=$(gopls version 2>/dev/null | head -1)
        log_success "Installed - $version"
        ;;
    1)
        log_error "Not found"
        log_info "  Install with: go install golang.org/x/tools/gopls@latest"
        required_failures=$((required_failures + 1))
        overall_success=false
        ;;
    2)
        log_warning "Found but not responding correctly"
        log_info "  Try reinstalling: go install golang.org/x/tools/gopls@latest"
        required_failures=$((required_failures + 1))
        overall_success=false
        ;;
esac

# Python (pyright)
echo -n "Python (pyright): "
case $(test_lsp_server "pyright" "pyright" "--version"; echo $?) in
    0)
        version=$(pyright --version 2>/dev/null)
        log_success "Installed - $version"
        ;;
    1)
        log_error "Not found"
        log_info "  Install with: npm install -g pyright"
        required_failures=$((required_failures + 1))
        overall_success=false
        ;;
    2)
        log_warning "Found but not responding correctly"
        log_info "  Try reinstalling: npm install -g pyright"
        required_failures=$((required_failures + 1))
        overall_success=false
        ;;
esac

# TypeScript Language Server
echo -n "TypeScript LSP: "
case $(test_lsp_server "typescript-language-server" "typescript-language-server" "--version"; echo $?) in
    0)
        version=$(typescript-language-server --version 2>/dev/null)
        log_success "Installed - $version"
        ;;
    1)
        log_error "Not found"
        log_info "  Install with: npm install -g typescript-language-server typescript"
        required_failures=$((required_failures + 1))
        overall_success=false
        ;;
    2)
        log_warning "Found but not responding correctly"
        log_info "  Try reinstalling: npm install -g typescript-language-server typescript"
        required_failures=$((required_failures + 1))
        overall_success=false
        ;;
esac

# TypeScript Compiler
echo -n "TypeScript Compiler: "
if command_exists tsc; then
    version=$(tsc --version 2>/dev/null)
    log_success "Installed - $version"
else
    log_error "Not found"
    log_info "  Install with: npm install -g typescript"
    required_failures=$((required_failures + 1))
    overall_success=false
fi

# Prettier
echo -n "Prettier: "
if command_exists prettier; then
    version=$(prettier --version 2>/dev/null)
    log_success "Installed - $version"
else
    log_error "Not found"
    log_info "  Install with: npm install -g prettier"
    required_failures=$((required_failures + 1))
    overall_success=false
fi

echo ""
echo "=== Optional Language Support ==="

# Erlang LSP
echo -n "Erlang (erlang_ls): "
case $(test_lsp_server "erlang_ls" "erlang_ls" "--help"; echo $?) in
    0)
        log_success "Installed and working"
        ;;
    1)
        log_warning "Not found (optional)"
        log_info "  See LSP_SETUP.md for installation instructions"
        optional_failures=$((optional_failures + 1))
        ;;
    2)
        log_warning "Found but not responding correctly (optional)"
        log_info "  See LSP_SETUP.md for troubleshooting"
        optional_failures=$((optional_failures + 1))
        ;;
esac

# Elixir LSP
echo -n "Elixir (elixir-ls): "
if command_exists elixir-ls; then
    log_success "Installed"
elif [ -x "/opt/elixir-ls/language_server.sh" ]; then
    log_success "Installed at /opt/elixir-ls/"
else
    log_warning "Not found (optional)"
    log_info "  See LSP_SETUP.md for installation instructions"
    optional_failures=$((optional_failures + 1))
fi

echo ""
echo "=== Development Environment ==="

# Programming language runtimes
echo -n "Go runtime: "
if command_exists go; then
    version=$(go version)
    gopath=$(go env GOPATH 2>/dev/null || echo "Not set")
    log_success "$version"
    log_info "  GOPATH: $gopath"
    
    # Check if GOPATH/bin is in PATH
    if [[ ":$PATH:" == *":$(go env GOPATH)/bin:"* ]]; then
        log_info "  Go bin directory is in PATH ✓"
    else
        log_warning "  Go bin directory not in PATH"
        log_info "  Add to ~/.bashrc: export PATH=\$PATH:\$(go env GOPATH)/bin"
    fi
else
    log_error "Go runtime not found"
    log_info "  Install Go from https://golang.org/dl/"
    required_failures=$((required_failures + 1))
    overall_success=false
fi

echo -n "Node.js: "
if command_exists node; then
    node_version=$(node --version)
    npm_version=$(npm --version)
    log_success "Node $node_version, npm $npm_version"
    
    # Check npm global directory
    npm_prefix=$(npm config get prefix 2>/dev/null || echo "")
    if [[ "$npm_prefix" == "$HOME/.npm-global" ]] || [[ ":$PATH:" == *":$HOME/.npm-global/bin:"* ]]; then
        log_info "  npm global directory configured ✓"
    else
        log_warning "  npm global directory not configured for user install"
        log_info "  Run: mkdir ~/.npm-global && npm config set prefix '~/.npm-global'"
        log_info "  Add to PATH: export PATH=~/.npm-global/bin:\$PATH"
    fi
else
    log_error "Node.js not found"
    log_info "  Install Node.js from https://nodejs.org/ or use your package manager"
    required_failures=$((required_failures + 1))
    overall_success=false
fi

echo -n "Python 3: "
if command_exists python3; then
    version=$(python3 --version)
    log_success "$version"
    
    # Check pip
    if command_exists pip3; then
        log_info "  pip3 available ✓"
    else
        log_warning "  pip3 not found"
        log_info "  Install with your package manager: apt install python3-pip"
    fi
else
    log_error "Python 3 not found"
    log_info "  Install Python 3 with your package manager"
    required_failures=$((required_failures + 1))
    overall_success=false
fi

# Optional: Erlang/Elixir
echo -n "Erlang: "
if command_exists erl; then
    version=$(erl -eval 'io:format("~s", [erlang:system_info(system_version)]), halt().' 2>/dev/null | head -1 | tr -d '\n')
    log_success "$version"
else
    log_warning "Not found (optional for Erlang development)"
fi

echo -n "Elixir: "
if command_exists elixir; then
    version=$(elixir --version 2>/dev/null | head -1)
    log_success "$version"
else
    log_warning "Not found (optional for Elixir development)"
fi

echo ""
echo "=== Additional Development Tools ==="

# Git
echo -n "Git: "
if command_exists git; then
    version=$(git --version)
    log_success "$version"
else
    log_warning "Git not found"
    log_info "  Install with your package manager"
fi

# Python development tools
echo -n "Python tools: "
python_tools=()
for tool in black isort autopep8 flake8 mypy pytest; do
    if command_exists "$tool" || python3 -c "import $tool" 2>/dev/null; then
        python_tools+=("$tool")
    fi
done

if [ ${#python_tools[@]} -gt 0 ]; then
    log_success "Found: ${python_tools[*]}"
else
    log_warning "No Python development tools found"
    log_info "  Install with: pip3 install --user black isort autopep8 flake8 mypy pytest"
fi

echo ""
echo "=== Summary ==="

if [ $required_failures -eq 0 ]; then
    log_success "All required LSP servers are installed and working!"
else
    log_error "$required_failures required LSP server(s) missing or not working"
    overall_success=false
fi

if [ $optional_failures -gt 0 ]; then
    log_warning "$optional_failures optional LSP server(s) not available"
fi

echo ""

if [ "$overall_success" = true ]; then
    log_success "✓ Your development environment is ready for Emacs!"
    echo ""
    echo "Next steps:"
    echo "1. Open Emacs and try opening a file in a supported language"
    echo "2. LSP should automatically start when you open .go, .py, .js, .ts files"
    echo "3. Use M-x my/show-package-stats in Emacs to see package loading times"
    echo "4. Check LSP status in Emacs with M-x lsp-describe-session"
    exit 0
else
    log_error "✗ Some required components are missing"
    echo ""
    echo "To fix issues:"
    echo "1. Run the installation script: ./install-lsp-servers.sh"
    echo "2. Or install missing components manually (see above)"
    echo "3. Restart your shell: source ~/.bashrc"
    echo "4. Run this verification script again"
    echo ""
    echo "For detailed setup instructions, see: LSP_SETUP.md"
    exit 1
fi