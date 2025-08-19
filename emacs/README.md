# Emacs Development Environment

A comprehensive Emacs configuration optimized for modern software development with full IDE features for Go, Python, JavaScript/TypeScript, Erlang, and Elixir.

## Quick Start

1. **Install LSP servers** (required for IDE features):
   ```bash
   ./install-lsp-servers.sh --erlang  # Include Erlang/Elixir (optional)
   ```

2. **Verify installation**:
   ```bash
   ./verify-lsp-setup.sh
   ```

3. **Use the configuration**:
   - The `init.el` should be symlinked to `~/.emacs.d/init.el`
   - Start Emacs - packages will auto-install and configure

## Files Overview

| File | Description |
|------|-------------|
| `init.el` | Main Emacs configuration with optimized startup |
| `LSP_SETUP.md` | Comprehensive LSP server installation guide |
| `CHEAT_SHEET.md` | Complete keyboard shortcuts and features reference |
| `install-lsp-servers.sh` | Automated LSP server installation script |
| `verify-lsp-setup.sh` | LSP server verification script |
| `setup-emacs.sh` | Emacs installation and setup script |

## Features

### Performance Optimizations
- ⚡ **Fast startup** with deferred package loading
- 📦 **Package caching** to avoid MELPA queries on startup  
- 🔧 **Profiling tools** to monitor loading times (`M-x my/show-package-stats`)

### Language Support
- **Go**: Full LSP with `gopls`, debugging, testing
- **Python**: Type checking with Pyright, Black formatting, pytest integration
- **JavaScript/TypeScript**: IntelliSense, Prettier formatting, ESLint
- **Erlang/Elixir**: LSP support with `erlang_ls` and `elixir-ls`
- **Web**: HTML, CSS, JSX/TSX with syntax highlighting

### Development Tools
- 🔍 **LSP Integration**: Full IDE features across all languages
- 📁 **Project Management**: Projectile for project-wide operations  
- 🌳 **File Navigation**: Treemacs file tree
- 🔀 **Git Integration**: Magit for version control
- 🖥️ **Terminal**: VTerm for full terminal emulation
- 🪟 **Window Manager**: EXWM integration (optional)

### Editor Enhancements
- ✏️ **Auto-completion**: Company mode with language-aware suggestions
- 🐛 **Syntax Checking**: Flycheck with real-time error detection
- 🎨 **Theme**: Zenburn color scheme
- 📖 **Documentation**: Which-key for command discovery
- 🔧 **Debugging**: DAP mode for step-through debugging

## Quick Commands

### Essential Shortcuts
- `C-c L` - Check LSP server status
- `C-x g` - Open Magit (Git interface)
- `C-c p` - Projectile commands (project management)
- `M-0` - Focus file tree (Treemacs)
- `C-c t` - Open terminal (VTerm)

### Language-Specific
- **Go**: `C-c g t` - Run tests
- **Python**: `C-c C-p` - Run buffer, `C-c p v` - Activate venv
- **JavaScript**: `C-c n d` - Start dev server
- **All languages**: `C-c f` - Format buffer, `C-c a` - Code actions

See `CHEAT_SHEET.md` for the complete reference.

## Installation Requirements

### System Dependencies
```bash
# Core tools (auto-installed by script)
- Node.js + npm (for Python/JS/TS LSP servers)
- Go (for Go LSP server) 
- Python 3 + pip (for development tools)
- Git (for version control)

# Optional (for Erlang/Elixir development)
- Erlang/OTP
- Elixir
```

### Distribution Support
The installation script supports:
- ✅ Ubuntu/Debian 18.04+
- ✅ Fedora/RHEL/CentOS 7+
- ✅ Arch Linux/Manjaro
- ✅ openSUSE Leap/Tumbleweed

## Troubleshooting

### LSP Issues
1. **Check server status**: `C-c L` in Emacs or `./verify-lsp-setup.sh`
2. **Enable debug logging**: Add `(setq lsp-log-io t)` to init.el
3. **Restart LSP**: `M-x lsp-workspace-restart`
4. **Check PATH**: Ensure LSP servers are in your shell's PATH

### Startup Issues  
1. **Package errors**: `M-x package-refresh-contents`
2. **Performance**: `M-x my/show-package-stats` to see slow packages
3. **Clean start**: Delete `~/.emacs.d/elpa/` and restart Emacs

### Installation Issues
1. **Permission errors**: Don't run scripts as root - they use sudo when needed
2. **Network issues**: Check internet connection for package downloads
3. **Missing dependencies**: Install build tools for your distribution

## Customization

The configuration is designed to be:
- **Modular**: Each language has its own section
- **Extensible**: Easy to add new languages or tools
- **Performant**: Optimized for fast startup and runtime

To customize:
1. Modify `init.el` sections for specific languages
2. Add custom functions with `my/` prefix
3. Use `use-package` with `:defer t` for new packages
4. Update `CHEAT_SHEET.md` with new shortcuts

## Contributing

When adding features:
1. Follow the existing code organization
2. Use deferred loading (`:defer t`) for non-essential packages
3. Add shortcuts to the appropriate sections
4. Update documentation in `CHEAT_SHEET.md`
5. Test with `./verify-lsp-setup.sh`

## Support

- 📖 **Full documentation**: See `LSP_SETUP.md` and `CHEAT_SHEET.md`
- 🔧 **Installation help**: Run `./install-lsp-servers.sh --help`
- 🔍 **Verification**: Use `./verify-lsp-setup.sh` for diagnostics
- 🆘 **In-Emacs help**: `C-c L` for LSP status, `C-h` prefix for help system

---

This configuration provides a modern, full-featured development environment that rivals contemporary IDEs while maintaining the flexibility and power of Emacs.