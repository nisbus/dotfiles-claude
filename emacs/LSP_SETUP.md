# Language Server Protocol (LSP) Setup Guide

This document provides comprehensive instructions for installing and configuring Language Server Protocol (LSP) servers for all programming languages supported by this Emacs configuration.

## Table of Contents
- [Quick Setup Script](#quick-setup-script)
- [Supported Languages](#supported-languages)
- [Prerequisites](#prerequisites)
- [Language Server Installation](#language-server-installation)
  - [Go (gopls)](#go-gopls)
  - [Python (Pyright)](#python-pyright)
  - [JavaScript/TypeScript](#javascripttypescript)
  - [Erlang (erlang_ls)](#erlang-erlang_ls)
  - [Elixir (elixir-ls)](#elixir-elixir-ls)
- [Distribution-Specific Instructions](#distribution-specific-instructions)
- [Verification](#verification)
- [Troubleshooting](#troubleshooting)

---

## Quick Setup Script

For automated installation, use the provided script:

```bash
# Make executable and run
chmod +x install-lsp-servers.sh
./install-lsp-servers.sh
```

The script automatically detects your distribution and installs all necessary LSP servers and dependencies.

---

## Supported Languages

This Emacs configuration provides LSP support for:

| Language | LSP Server | Package Manager | Status |
|----------|------------|----------------|---------|
| **Go** | gopls | Go modules | ✅ Required |
| **Python** | Pyright | npm | ✅ Required |
| **JavaScript** | typescript-language-server | npm | ✅ Required |
| **TypeScript** | typescript-language-server | npm | ✅ Required |
| **Erlang** | erlang_ls | Manual/Package | 🔶 Optional |
| **Elixir** | elixir-ls | Manual/Package | 🔶 Optional |

---

## Prerequisites

### Required Package Managers

**Node.js and npm** (for JavaScript/TypeScript/Python LSP servers):
```bash
# Ubuntu/Debian
sudo apt update && sudo apt install -y nodejs npm

# Fedora/RHEL/CentOS
sudo dnf install -y nodejs npm

# Arch Linux
sudo pacman -S nodejs npm

# openSUSE
sudo zypper install nodejs npm
```

**Go** (for Go LSP server):
```bash
# Ubuntu/Debian (recent versions)
sudo apt install -y golang-go

# Fedora/RHEL/CentOS
sudo dnf install -y golang

# Arch Linux
sudo pacman -S go

# openSUSE
sudo zypper install go

# Manual installation (all distros)
# Download from https://golang.org/dl/
```

**Python 3** (usually pre-installed):
```bash
# Ubuntu/Debian
sudo apt install -y python3 python3-pip

# Fedora/RHEL/CentOS
sudo dnf install -y python3 python3-pip

# Arch Linux
sudo pacman -S python python-pip
```

---

## Language Server Installation

### Go (gopls)

**Installation:**
```bash
# Install gopls (Go's official language server)
go install golang.org/x/tools/gopls@latest

# Ensure Go bin directory is in PATH
echo 'export PATH=$PATH:$(go env GOPATH)/bin' >> ~/.bashrc
source ~/.bashrc
```

**Verification:**
```bash
gopls version
which gopls
```

**Features:**
- Code completion
- Go to definition/references
- Hover documentation
- Code formatting (gofmt/goimports)
- Code actions and refactoring
- Diagnostics and error checking

---

### Python (Pyright)

**Installation:**
```bash
# Install Pyright (Microsoft's Python language server)
sudo npm install -g pyright

# Alternative: Using pipx (recommended for Python tools)
python3 -m pip install --user pipx
pipx install pyright
```

**Verification:**
```bash
pyright --version
which pyright
```

**Features:**
- Type checking
- Code completion
- Import resolution
- Refactoring
- Documentation on hover
- Diagnostic messages

**Additional Python Tools:**
```bash
# Code formatters
pip install black isort autopep8

# Linting
pip install flake8 pylint mypy

# Testing
pip install pytest pytest-cov
```

---

### JavaScript/TypeScript

**Installation:**
```bash
# Install TypeScript language server
sudo npm install -g typescript-language-server typescript

# Install additional tools
sudo npm install -g prettier eslint
```

**Verification:**
```bash
typescript-language-server --version
tsc --version
prettier --version
```

**Features:**
- IntelliSense and autocompletion
- Error checking and diagnostics
- Refactoring support
- Import management
- Code formatting
- Quick fixes

**Project-Level Setup:**
```bash
# In your JavaScript/TypeScript project
npm install --save-dev @types/node
npm install --save-dev eslint @typescript-eslint/parser @typescript-eslint/eslint-plugin
```

---

### Erlang (erlang_ls)

**Installation Methods:**

#### Method 1: Package Manager (Recommended)
```bash
# Ubuntu/Debian (if available)
sudo apt install erlang-ls

# Fedora
sudo dnf install erlang-ls

# Arch Linux (AUR)
yay -S erlang-ls-git
```

#### Method 2: Manual Compilation
```bash
# Prerequisites
sudo apt install -y erlang-dev erlang-tools rebar3  # Ubuntu/Debian
sudo dnf install -y erlang-rebar3 erlang-tools       # Fedora

# Clone and build
git clone https://github.com/erlang-ls/erlang_ls.git
cd erlang_ls
make
sudo make install

# Or install to user directory
make install PREFIX=$HOME/.local
echo 'export PATH=$HOME/.local/bin:$PATH' >> ~/.bashrc
```

#### Method 3: Pre-built Binary
```bash
# Download latest release
curl -L -o erlang_ls https://github.com/erlang-ls/erlang_ls/releases/latest/download/erlang_ls
chmod +x erlang_ls
sudo mv erlang_ls /usr/local/bin/
```

**Verification:**
```bash
erlang_ls --version
which erlang_ls
```

---

### Elixir (elixir-ls)

**Installation:**

#### Method 1: Package Manager
```bash
# Ubuntu/Debian (if available in repos)
sudo apt install elixir-ls

# Arch Linux (AUR)
yay -S elixir-ls
```

#### Method 2: Manual Installation
```bash
# Prerequisites: Install Elixir and Erlang
sudo apt install -y elixir    # Ubuntu/Debian
sudo dnf install -y elixir    # Fedora

# Clone and build elixir-ls
git clone https://github.com/elixir-lsp/elixir-ls.git
cd elixir-ls
mix deps.get
MIX_ENV=prod mix compile
MIX_ENV=prod mix elixir_ls.release -o release

# Install to system
sudo cp -r release /opt/elixir-ls
sudo ln -sf /opt/elixir-ls/language_server.sh /usr/local/bin/elixir-ls
```

**Verification:**
```bash
elixir-ls --help
elixir --version
```

---

## Distribution-Specific Instructions

### Ubuntu/Debian 20.04+

```bash
#!/bin/bash
# Complete setup for Ubuntu/Debian

# Update package list
sudo apt update

# Install prerequisites
sudo apt install -y curl wget git build-essential

# Node.js and npm (NodeSource repository for latest version)
curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -
sudo apt install -y nodejs

# Go (latest version)
sudo apt install -y golang-go

# Python tools
sudo apt install -y python3 python3-pip python3-venv

# Erlang/Elixir
sudo apt install -y erlang elixir

# LSP Servers
go install golang.org/x/tools/gopls@latest
sudo npm install -g pyright typescript-language-server typescript prettier

# Optional: Build erlang_ls from source
# (Package version might be outdated)
```

### Fedora/RHEL/CentOS

```bash
#!/bin/bash
# Complete setup for Fedora/RHEL/CentOS

# Update system
sudo dnf update -y

# Install prerequisites
sudo dnf install -y curl wget git gcc make

# Development tools group
sudo dnf groupinstall -y "Development Tools"

# Node.js and npm
sudo dnf install -y nodejs npm

# Go
sudo dnf install -y golang

# Python tools
sudo dnf install -y python3 python3-pip

# Erlang/Elixir
sudo dnf install -y erlang elixir

# LSP Servers
go install golang.org/x/tools/gopls@latest
sudo npm install -g pyright typescript-language-server typescript prettier
```

### Arch Linux

```bash
#!/bin/bash
# Complete setup for Arch Linux

# Update system
sudo pacman -Syu --noconfirm

# Install prerequisites
sudo pacman -S --noconfirm base-devel git curl wget

# Programming languages and tools
sudo pacman -S --noconfirm nodejs npm go python python-pip erlang elixir

# LSP Servers
go install golang.org/x/tools/gopls@latest
sudo npm install -g pyright typescript-language-server typescript prettier

# AUR packages (requires AUR helper like yay)
# yay -S erlang-ls-git elixir-ls
```

### openSUSE

```bash
#!/bin/bash
# Complete setup for openSUSE

# Update system
sudo zypper refresh && sudo zypper update -y

# Install prerequisites
sudo zypper install -y git curl wget gcc make

# Development pattern
sudo zypper install -y -t pattern devel_basis

# Programming languages
sudo zypper install -y nodejs npm go python3 python3-pip erlang

# LSP Servers
go install golang.org/x/tools/gopls@latest
sudo npm install -g pyright typescript-language-server typescript prettier
```

---

## Verification

After installation, verify all LSP servers are working:

```bash
#!/bin/bash
# Verification script

echo "=== LSP Server Verification ==="

echo -n "Go (gopls): "
if command -v gopls &> /dev/null; then
    echo "✅ $(gopls version)"
else
    echo "❌ Not found"
fi

echo -n "Python (pyright): "
if command -v pyright &> /dev/null; then
    echo "✅ $(pyright --version)"
else
    echo "❌ Not found"
fi

echo -n "TypeScript: "
if command -v typescript-language-server &> /dev/null; then
    echo "✅ $(typescript-language-server --version)"
else
    echo "❌ Not found"
fi

echo -n "Prettier: "
if command -v prettier &> /dev/null; then
    echo "✅ $(prettier --version)"
else
    echo "❌ Not found"
fi

echo -n "Erlang (erlang_ls): "
if command -v erlang_ls &> /dev/null; then
    echo "✅ Found"
else
    echo "⚠️  Not found (optional)"
fi

echo -n "Elixir (elixir-ls): "
if command -v elixir-ls &> /dev/null; then
    echo "✅ Found"
else
    echo "⚠️  Not found (optional)"
fi

echo ""
echo "=== Environment Check ==="
echo "Go path: $(go env GOPATH 2>/dev/null || echo 'Not set')"
echo "Node version: $(node --version 2>/dev/null || echo 'Not found')"
echo "Python version: $(python3 --version 2>/dev/null || echo 'Not found')"
```

---

## Troubleshooting

### Common Issues

#### LSP Server Not Found
**Problem**: Emacs can't find the LSP server executable.
**Solution**:
```bash
# Check if server is in PATH
which gopls pyright typescript-language-server

# Add to PATH if needed (Go example)
echo 'export PATH=$PATH:$(go env GOPATH)/bin' >> ~/.bashrc
source ~/.bashrc
```

#### Permission Errors with npm
**Problem**: Permission denied when installing global npm packages.
**Solutions**:
```bash
# Option 1: Use npm prefix (recommended)
mkdir ~/.npm-global
npm config set prefix '~/.npm-global'
echo 'export PATH=~/.npm-global/bin:$PATH' >> ~/.bashrc
source ~/.bashrc
npm install -g pyright typescript-language-server typescript

# Option 2: Fix npm permissions
sudo chown -R $(whoami) $(npm config get prefix)/{lib/node_modules,bin,share}

# Option 3: Use nvm (Node Version Manager)
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
source ~/.bashrc
nvm install node
npm install -g pyright typescript-language-server typescript
```

#### Go Module Issues
**Problem**: Go LSP server not working in projects.
**Solution**:
```bash
# Initialize Go module if needed
cd your-project
go mod init your-project-name
go mod tidy

# Set GOPATH if using old-style Go workspace
export GOPATH=$HOME/go
```

#### Python Import Resolution
**Problem**: Pyright can't find Python modules.
**Solutions**:
```bash
# Option 1: Use virtual environments
python3 -m venv venv
source venv/bin/activate
pip install your-packages

# Option 2: Create pyrightconfig.json in project root
cat > pyrightconfig.json << EOF
{
    "venvPath": ".",
    "venv": "venv",
    "pythonPath": "./venv/bin/python"
}
EOF

# Option 3: Set Python path in Emacs
# Add to init.el:
# (setq lsp-pyright-python-executable-cmd "python3")
```

#### Erlang/Elixir LSP Issues
**Problem**: erlang_ls or elixir-ls not working.
**Solutions**:
```bash
# Check Erlang/Elixir installation
erl -eval 'io:format("~s~n", [erlang:system_info(system_version)]), halt().'
elixir --version

# Build with correct OTP version
export ERL_FLAGS="+pc unicode"
# Then rebuild erlang_ls/elixir-ls
```

### Emacs LSP Configuration Check

Add this to your init.el to debug LSP issues:

```elisp
;; Temporary debugging settings
(setq lsp-log-io t)  ; Enable to see LSP communication
(setq lsp-print-performance t)  ; Show performance info

;; Check LSP server paths
(defun my/check-lsp-servers ()
  "Check if LSP servers are available."
  (interactive)
  (let ((servers '(("gopls" . "Go")
                   ("pyright" . "Python") 
                   ("typescript-language-server" . "TypeScript")
                   ("erlang_ls" . "Erlang")
                   ("elixir-ls" . "Elixir"))))
    (dolist (server servers)
      (message "%s (%s): %s" 
               (cdr server)
               (car server)
               (if (executable-find (car server)) "✅ Found" "❌ Not found")))))

;; Call with M-x my/check-lsp-servers
```

---

## Performance Tips

### Reducing LSP Server Memory Usage

```bash
# For large projects, you might want to tune LSP servers

# Go: Reduce memory usage
export GOMEMLIMIT=1GiB  # Limit Go LSP memory

# Python: Reduce Pyright memory usage
# Add to pyrightconfig.json:
# {
#   "typeCheckingMode": "basic",
#   "useLibraryCodeForTypes": false
# }

# TypeScript: Reduce memory usage
# Add to tsconfig.json:
# {
#   "compilerOptions": {
#     "disableSizeLimit": false,
#     "maxNodeModuleJsDepth": 1
#   }
# }
```

### Speeding Up LSP Initialization

```elisp
;; Add to init.el for faster LSP startup
(setq lsp-auto-guess-root t)
(setq lsp-document-sync-method 'incremental)
(setq lsp-response-timeout 10)
(setq lsp-idle-delay 0.5)
```

---

*This setup provides a complete, production-ready development environment with full IDE features for all supported programming languages.*