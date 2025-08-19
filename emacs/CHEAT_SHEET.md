# Emacs Development Environment Cheat Sheet

This cheat sheet covers all the custom shortcuts and features configured in this Emacs setup for Go, Erlang, JavaScript/TypeScript, Python development, and EXWM window management.

## Table of Contents
- [Global Shortcuts](#global-shortcuts)
- [Editor Enhancements](#editor-enhancements)
- [LSP (Language Server Protocol)](#lsp-language-server-protocol)
- [Go Development](#go-development)
- [Erlang Development](#erlang-development)
- [JavaScript/TypeScript Development](#javascripttypescript-development)
- [Python Development](#python-development)
- [Git Integration (Magit)](#git-integration-magit)
- [Project Management (Projectile)](#project-management-projectile)
- [File Navigation (Treemacs)](#file-navigation-treemacs)
- [Terminal Integration](#terminal-integration)
- [EXWM Window Manager](#exwm-window-manager)
- [Package Management](#package-management)

---

## Global Shortcuts

### Basic Navigation & Editing
| Shortcut | Command | Description |
|----------|---------|-------------|
| `C-x C-b` | `ibuffer` | Enhanced buffer list |
| `M-/` | `company-complete` | Manual auto-completion |
| `C-g` | `keyboard-quit` | Cancel current command |

### Compilation & Building
| Shortcut | Command | Description |
|----------|---------|-------------|
| `C-c c` | `compile` | Compile current project |
| `C-c r` | `recompile` | Recompile with last command |

### Custom Project Functions
| Shortcut | Command | Description |
|----------|---------|-------------|
| `C-c g t` | `my/run-go-tests` | Run Go tests for project |
| `C-c n d` | `my/run-npm-dev` | Run npm dev server |
| `C-c n b` | `my/run-npm-build` | Run npm build |
| `C-c p t` | `my/run-python-tests` | Run Python tests |
| `C-c p r` | `my/run-python-file` | Run current Python file |
| `C-c p v` | `my/activate-python-venv` | Activate Python virtual environment |

---

## Editor Enhancements

### Company Mode (Auto-completion)
- **Automatic**: Triggers after 0.2 seconds with 1+ characters
- **Manual**: `M-/` to force completion
- **Navigation**: `C-n`/`C-p` or arrow keys in completion popup

### Flycheck (Syntax Checking)
- **Automatic**: Real-time syntax checking
- **Navigate errors**: `C-c ! n` (next), `C-c ! p` (previous)
- **List errors**: `C-c ! l`

### Which-key
- **Automatic**: Shows available key combinations after partial input
- **Delay**: 1 second after typing prefix

---

## LSP (Language Server Protocol)

### Core LSP Commands
| Shortcut | Command | Description |
|----------|---------|-------------|
| `C-c l` | LSP prefix | Shows all LSP commands |
| `C-c d` | `lsp-describe-thing-at-point` | Show documentation |
| `C-c a` | `lsp-execute-code-action` | Execute code action |
| `C-c f` | `lsp-format-buffer` | Format current buffer |
| `C-c s` | `lsp-workspace-symbol` | Search workspace symbols |
| `C-c L` | `my/check-lsp-servers` | Check LSP server status |

### LSP Navigation
| Shortcut | Command | Description |
|----------|---------|-------------|
| `M-.` | `lsp-find-definition` | Go to definition |
| `M-?` | `lsp-find-references` | Find references |
| `C-c l r` | `lsp-rename` | Rename symbol |
| `C-c l h` | `lsp-signature-help` | Show function signature |

### LSP UI Features
- **Documentation**: Hover over symbols for documentation
- **Sideline**: Shows type info and diagnostics
- **Peek**: `C-c l p` for peek definition/references

---

## Go Development

### Go Mode Shortcuts
| Shortcut | Command | Description |
|----------|---------|-------------|
| `C-c C-r` | `go-remove-unused-imports` | Remove unused imports |
| `C-c C-g` | `go-goto-imports` | Jump to imports section |
| `C-c C-k` | `godoc` | Show Go documentation |
| `C-c g t` | `my/run-go-tests` | Run all Go tests |

### Go Features
- **Auto-formatting**: Uses `goimports` on save if available
- **LSP Integration**: Full Go language server support
- **Debugging**: DAP mode configured for Go debugging
- **Compilation**: `go build -v && go test -v && go vet`

### Go DAP Debugging
- **Breakpoints**: `C-c l d b` to toggle breakpoint
- **Start Debug**: `C-c l d d` to start debugging
- **Step Over**: `C-c l d n`
- **Step Into**: `C-c l d i`

---

## Erlang Development

### Erlang Mode Features
- **File Extensions**: `.erl`, `.hrl`, `.xrl`, `.yrl`, `.app.src`, `rebar.config`
- **LSP Integration**: Uses `erlang_ls` if available
- **Shell History**: Saved to `~/.erlang_history` (10,000 entries)
- **Auto-detection**: Finds Erlang installation automatically

### Erlang Paths Checked
1. `/usr/lib/erlang`
2. `/usr/local/lib/erlang`
3. `~/.guix-profile/lib/erlang`

### Elixir Support
- **File Extensions**: `.ex`, `.exs`, `.eex`
- **Alchemist**: Enhanced Elixir development tools
- **LSP Integration**: Full Elixir language server support

---

## JavaScript/TypeScript Development

### JS2 Mode (JavaScript)
| Shortcut | Feature | Description |
|----------|---------|-------------|
| Auto-format | Prettier | Formats on save |
| Indentation | 2 spaces | Consistent spacing |
| LSP | Full support | IntelliSense, refactoring |

### TypeScript Mode
- **File Extension**: `.ts`
- **Indentation**: 2 spaces
- **LSP Integration**: Full TypeScript language server
- **Auto-completion**: Company mode integration

### Web Mode (JSX/TSX)
- **File Extensions**: `.tsx`, `.jsx`, `.html`, `.css`, `.scss`
- **Features**: Auto-pairing, CSS colorization, element highlighting
- **Indentation**: 2 spaces for all web content

### JSON Mode
- **File Extension**: `.json`
- **Formatting**: 2-space indentation
- **Prettier**: Auto-formatting on save

### NPM Integration
| Shortcut | Command | Description |
|----------|---------|-------------|
| `C-c n d` | `my/run-npm-dev` | Start development server |
| `C-c n b` | `my/run-npm-build` | Build for production |

---

## Python Development

### Python Mode Shortcuts
| Shortcut | Command | Description |
|----------|---------|-------------|
| `C-c C-p` | `my/python-run-buffer` | Run current Python buffer |
| `C-c C-t` | `my/python-run-pytest-file` | Run pytest on current file |
| `C-c C-z` | `python-shell-switch-to-shell` | Switch to Python REPL |
| `C-c i` | `my/python-add-import` | Add import statement |

### Python Testing (pytest)
| Shortcut | Command | Description |
|----------|---------|-------------|
| `C-c t t` | `python-pytest-run-all` | Run all tests |
| `C-c t f` | `python-pytest-run-file` | Run tests in current file |
| `C-c t F` | `python-pytest-run-function` | Run test at point |
| `C-c t r` | `python-pytest-repeat` | Repeat last test run |
| `C-c t p` | `python-pytest-popup` | Pytest options popup |

### Python Global Shortcuts
| Shortcut | Command | Description |
|----------|---------|-------------|
| `C-c p t` | `my/run-python-tests` | Run Python tests (global) |
| `C-c p r` | `my/run-python-file` | Run current Python file (global) |
| `C-c p v` | `my/activate-python-venv` | Activate virtual environment |

### Python Features
- **Auto-formatting**: Black formatter (88 char line length)
- **Import sorting**: isort on save
- **Virtual environments**: Auto-detection of `.venv` directories
- **LSP**: Pyright language server with full IntelliSense
- **Debugging**: DAP mode with debugpy integration
- **REPL**: IPython integration when available
- **Jupyter**: EIN notebook support
- **Docstrings**: Enhanced docstring editing

### Python DAP Debugging
- **Setup**: Automatically configured with debugpy
- **Templates**: "My App" debug template available
- **Breakpoints**: Standard DAP debugging shortcuts

---

## Git Integration (Magit)

### Magit Shortcuts
| Shortcut | Command | Description |
|----------|---------|-------------|
| `C-x g` | `magit-status` | Open Magit status buffer |

### Magit Status Buffer Commands
| Key | Command | Description |
|-----|---------|-------------|
| `s` | Stage file/hunk | Stage changes |
| `u` | Unstage file/hunk | Unstage changes |
| `c c` | Commit | Create commit |
| `P p` | Push | Push to remote |
| `F p` | Pull | Pull from remote |
| `b b` | Switch branch | Change branch |
| `b c` | Create branch | Create new branch |
| `l l` | Log | View commit history |
| `d d` | Diff | View diff |
| `q` | Quit | Close Magit buffer |

---

## Project Management (Projectile)

### Projectile Shortcuts
| Shortcut | Command | Description |
|----------|---------|-------------|
| `C-c p` | Projectile prefix | Shows all Projectile commands |
| `C-c p f` | `projectile-find-file` | Find file in project |
| `C-c p p` | `projectile-switch-project` | Switch to another project |
| `C-c p s g` | `projectile-grep` | Grep in project |
| `C-c p r` | `projectile-replace` | Replace in project |
| `C-c p c` | `projectile-compile-project` | Compile project |
| `C-c p t` | `projectile-test-project` | Test project |

### Projectile Features
- **Auto-detection**: Recognizes Git repos, language-specific files
- **Caching**: Improves performance for large projects
- **Integration**: Works with all supported languages

---

## File Navigation (Treemacs)

### Treemacs Shortcuts
| Shortcut | Command | Description |
|----------|---------|-------------|
| `M-0` | `treemacs-select-window` | Focus Treemacs window |
| `C-x t 1` | `treemacs-delete-other-windows` | Show only Treemacs |
| `C-x t t` | `treemacs` | Toggle Treemacs |
| `C-x t B` | `treemacs-bookmark` | Bookmark current location |
| `C-x t C-t` | `treemacs-find-file` | Find current file in tree |
| `C-x t M-t` | `treemacs-find-tag` | Find tag in tree |

### Treemacs Buffer Commands
| Key | Command | Description |
|-----|---------|-------------|
| `RET` | Open file | Open file or expand directory |
| `o` | Open file other window | Open in other window |
| `TAB` | Expand/collapse | Toggle directory |
| `r` | Refresh | Refresh tree view |
| `c` | Create file | Create new file |
| `d` | Delete | Delete file/directory |
| `R` | Rename | Rename file/directory |

---

## Terminal Integration

### VTerm
| Shortcut | Command | Description |
|----------|---------|-------------|
| `C-c t` | `vterm` | Open terminal in Emacs |

### VTerm Features
- **Full terminal emulation**: Better than term/ansi-term
- **Integration**: Copy/paste, directory tracking
- **Performance**: Fast terminal emulation

---

## EXWM Window Manager

### Global EXWM Shortcuts (when running as WM)
| Shortcut | Command | Description |
|----------|---------|-------------|
| `s-r` | `exwm-reset` | Reset to line mode |
| `s-w` | `exwm-workspace-switch` | Switch workspace |
| `s-&` | Launch application | Run shell command |
| `s-0` to `s-9` | Switch workspace | Go to workspace N |

### EXWM Features
- **Workspaces**: 4 workspaces by default
- **Window management**: Firefox/Chromium → workspace 1, Emacs → workspace 0
- **Integration**: Full Emacs integration with X11 applications

---

## Package Management

### Use-package Features
- **Automatic installation**: All packages auto-install from MELPA/ELPA
- **Lazy loading**: Packages load only when needed
- **Configuration**: Centralized package configuration

### Package Sources
1. **MELPA**: `https://melpa.org/packages/` (primary)
2. **ELPA**: `https://elpa.gnu.org/packages/` (fallback)

### Installed Packages by Category

#### Core Editor
- `company` - Auto-completion
- `flycheck` - Syntax checking
- `which-key` - Key binding help
- `zenburn-theme` - Color theme

#### Development Tools
- `lsp-mode` - Language Server Protocol
- `lsp-ui` - LSP UI enhancements
- `lsp-treemacs` - LSP + Treemacs integration
- `dap-mode` - Debug Adapter Protocol
- `magit` - Git integration
- `projectile` - Project management
- `treemacs` - File tree navigation

#### Language Support
- **Go**: `go-mode`
- **Erlang**: `erlang`, `elixir-mode`, `alchemist`
- **JavaScript/TypeScript**: `js2-mode`, `typescript-mode`, `web-mode`, `json-mode`, `prettier`, `npm-mode`
- **Python**: `python-mode`, `lsp-pyright`, `pyvenv`, `blacken`, `py-isort`, `python-pytest`, `ein`, `python-docstring`

#### System Integration
- `vterm` - Terminal emulator
- `yaml-mode` - YAML support
- `dockerfile-mode` - Docker support
- `docker-compose-mode` - Docker Compose support

---

## Tips & Best Practices

### Performance
- **Startup time**: Configuration optimized for fast loading with deferred package loading
- **Package caching**: MELPA queries cached to avoid network delays on startup
- **GC tuning**: Garbage collection optimized for development
- **Lazy loading**: Most packages load only when needed (`:defer t`)
- **Profiling**: Use `M-x my/show-package-stats` to see package loading times

### Customization
- **Custom functions**: All custom functions prefixed with `my/`
- **Key bindings**: Organized by functionality and mode
- **Auto-formatting**: Most languages format on save

### Debugging
- **LSP servers**: Use `C-c L` or `M-x my/check-lsp-servers` to verify LSP server installation
- **LSP logs**: Set `lsp-log-io` to `t` for debugging LSP issues
- **LSP session**: Use `M-x lsp-describe-session` to check current LSP status
- **Package issues**: Use `M-x package-refresh-contents` if packages fail to install
- **Performance**: Use `M-x profiler-start` to profile slow operations

### Virtual Environments (Python)
- **Auto-activation**: Place `.venv` directory in project root
- **Manual activation**: `C-c p v` or `M-x pyvenv-activate`
- **Deactivation**: `M-x pyvenv-deactivate`

### Language Server Installation
- **Automated**: Run `./install-lsp-servers.sh` in the emacs directory
- **Manual installation**: See `LSP_SETUP.md` for detailed instructions
- **Verification**: Run `./verify-lsp-setup.sh` or use `C-c L` in Emacs
- **Required servers**: 
  - Go: `gopls` 
  - Python: `pyright`
  - JavaScript/TypeScript: `typescript-language-server`
- **Optional servers**: `erlang_ls`, `elixir-ls`

---

*This configuration provides a complete IDE experience for multi-language development with modern tooling and excellent integration between different development environments.*