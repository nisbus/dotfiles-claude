# Monitor Configuration

This directory contains scripts and configuration for automatic monitor detection and management using `autorandr`.

## Features

- **Automatic Detection**: Monitors are automatically detected when connected/disconnected
- **Profile Management**: Predefined profiles for common setups (laptop-only, dual, external-only)
- **XMonad Integration**: XMonad automatically restarts to update workspaces when monitors change
- **External as Primary**: When dual monitors are detected, the external monitor is set as primary

## Setup

The monitor configuration is automatically set up when you run the main `setup.sh` script. It will:

1. Install required packages (`xrandr`, `arandr`, `autorandr`)
2. Detect your current monitor configuration
3. Create autorandr profiles based on your setup
4. Configure XMonad integration hooks
5. Set up systemd service for automatic detection

## Manual Usage

### View available profiles
```bash
autorandr --list
```

### Switch profiles manually
```bash
autorandr laptop      # Laptop screen only
autorandr dual        # Both screens (external as primary)
autorandr external    # External monitor only
```

### Auto-detect and apply best profile
```bash
autorandr --change
```

### Save current configuration as a new profile
```bash
autorandr --save profile-name
```

## Files

- `setup-monitors.sh` - Main setup script that configures autorandr and creates profiles
- Profiles are stored in `~/.config/autorandr/`
- XMonad hook is at `~/.config/autorandr/postswitch`

## Troubleshooting

If monitors aren't switching automatically:

1. Check that autorandr service is running:
   ```bash
   systemctl --user status autorandr
   ```

2. Manually trigger detection:
   ```bash
   autorandr --change
   ```

3. Check current configuration:
   ```bash
   xrandr --query
   ```

4. Recreate profiles if needed:
   ```bash
   bash ~/dotfiles-claude/monitor/setup-monitors.sh
   ```