# Claude Code Configuration

This directory contains the installation script and configuration for Claude Code.

## Contents

- `install-claude.sh` - Installation script that:
  - Checks for and installs npm if needed
  - Configures npm to use user-level global packages (no sudo required)
  - Installs Claude Code
  - Sets up PATH for user-level npm binaries

- `dot-claude/` - Claude configuration directory containing:
  - Projects configurations
  - Shell snapshots
  - Todos
  - Other Claude settings

## Installation

The Claude installation is integrated into the main `install.sh` script. When you run the dotfiles installation, it will prompt you to install Claude Code.

### Manual Installation

If you want to install Claude separately:

```bash
cd ~/dotfiles/claude
./install-claude.sh
```

## Configuration

Your Claude configuration (agents, projects, etc.) will be automatically copied from `dot-claude/` to `~/.claude/` during installation.

**Note:** Credentials are not stored in the dotfiles. You'll need to authenticate Claude on each new machine using:

```bash
claude login
```

## Auto-Updates

Claude is installed in the user's npm directory (`~/.npm-global`), which means:
- No sudo required for updates
- Auto-updates will work without permission issues
- You can manually update with: `npm update -g @anthropic-ai/claude-code`