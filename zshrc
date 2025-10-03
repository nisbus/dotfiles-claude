# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$PATH

# Path to your Oh My Zsh installation.
export ZSH="$HOME/.oh-my-zsh"

source ~/.env

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time Oh My Zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    zsh-autosuggestions
    zsh-syntax-highlighting
    npm
    node
    docker
    docker-compose
    golang
    rust
    python
    pip
)

# Source Oh My Zsh only if it exists
if [ -f "$ZSH/oh-my-zsh.sh" ]; then
    source $ZSH/oh-my-zsh.sh
else
    echo "Oh My Zsh not found. Run ./dev-setup.sh to install it."
fi

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='emacs'
fi

# Compilation flags
# export ARCHFLAGS="-arch $(uname -m)"

# ============================================================================
# Development Environment Configuration
# ============================================================================

# Go configuration
export GOPATH="$HOME/go"
export PATH="$PATH:/usr/local/go/bin:$GOPATH/bin"

# Go environment variables
export GO111MODULE=on
export GOPROXY=https://proxy.golang.org,direct
export GOSUMDB=sum.golang.org

# Erlang/OTP configuration
export ERL_AFLAGS="-kernel shell_history enabled"

# Check for Erlang installations
if [ -d "/usr/lib/erlang" ]; then
    export PATH="$PATH:/usr/lib/erlang/bin"
elif [ -d "/usr/local/lib/erlang" ]; then
    export PATH="$PATH:/usr/local/lib/erlang/bin"
fi

# Enable kerl for Erlang version management (if installed)
[ -f "$HOME/.kerl/activate" ] && source "$HOME/.kerl/activate"
[ -f "$HOME/.kerlrc" ] && source "$HOME/.kerlrc"

# Node.js and npm configuration
export PATH="$PATH:$HOME/.npm-global/bin"
export NODE_OPTIONS="--max-old-space-size=4096"

# pnpm
export PNPM_HOME="$HOME/.local/share/pnpm"
export PATH="$PNPM_HOME:$PATH"

# Rust configuration (if installed)
[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"

# Python configuration
export PYTHONDONTWRITEBYTECODE=1
export PYTHONUNBUFFERED=1

expot PATH="/home/valdimar/.claude/local:$PATH"
# ============================================================================
# Aliases
# ============================================================================

# Erlang aliases
alias erl="erl -sname shell"
alias rebar="rebar3"
alias iex="iex --sname shell"

# Go aliases
alias gotest="go test -v ./..."
alias gocover="go test -coverprofile=coverage.out && go tool cover -html=coverage.out"
alias gomod="go mod tidy && go mod vendor"
alias gobuild="go build -v"
alias gorun="go run ."

# Node.js/npm aliases
alias ni="npm install"
alias nid="npm install --save-dev"
alias nig="npm install -g"
alias nr="npm run"
alias nrs="npm run start"
alias nrd="npm run dev"
alias nrb="npm run build"
alias nrt="npm run test"
alias nrl="npm run lint"

# Next.js aliases
alias next-dev="npx next dev"
alias next-build="npx next build"
alias next-start="npx next start"
alias next-lint="npx next lint"
alias create-next="npx create-next-app@latest"

# Docker aliases
alias d="docker"
alias dc="docker-compose"
alias dps="docker ps"
alias dpsa="docker ps -a"
alias di="docker images"
alias dex="docker exec -it"
alias dlog="docker logs -f"
alias drm="docker rm"
alias drmi="docker rmi"
alias dprune="docker system prune -a"

# Git aliases (additional to oh-my-zsh git plugin)
alias gs="git status"
alias gd="git diff"
alias gdc="git diff --cached"
alias gl="git log --oneline --graph --decorate"
alias gla="git log --oneline --graph --decorate --all"
alias gp="git push"
alias gpf="git push --force-with-lease"
alias gpl="git pull"
alias gco="git checkout"
alias gcb="git checkout -b"
alias gcm="git commit -m"
alias gca="git commit --amend"
alias grb="git rebase"
alias grbi="git rebase -i"
alias gst="git stash"
alias gstp="git stash pop"

# System aliases
alias ll="ls -alh"
alias la="ls -A"
alias l="ls -CF"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias ~="cd ~"
alias -- -="cd -"

# Clipboard aliases using xclip
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'
alias copy='xclip -selection clipboard'
alias paste='xclip -selection clipboard -o'

# Development utilities
alias ports="netstat -tulanp"
alias myip="curl http://ipecho.net/plain; echo"
alias weather="curl wttr.in"

# Claude aliases
alias c="claude --dangerously-skip-permissions"
# ============================================================================
# Functions
# ============================================================================

# Create a new Go module
gomod-init() {
    if [ -z "$1" ]; then
        echo "Usage: gomod-init <module-name>"
        return 1
    fi
    go mod init "$1"
    go mod tidy
}

# Create a new Next.js project with TypeScript
next-init() {
    if [ -z "$1" ]; then
        echo "Usage: next-init <project-name>"
        return 1
    fi
    npx create-next-app@latest "$1" --typescript --tailwind --eslint --app
}

# Quick git commit and push
gquick() {
    if [ -z "$1" ]; then
        echo "Usage: gquick <commit-message>"
        return 1
    fi
    git add .
    git commit -m "$1"
    git push
}

# Docker container shell
dsh() {
    if [ -z "$1" ]; then
        echo "Usage: dsh <container-name-or-id>"
        return 1
    fi
    docker exec -it "$1" /bin/bash || docker exec -it "$1" /bin/sh
}

# Extract various archive formats
extract() {
    if [ -f "$1" ] ; then
        case "$1" in
            *.tar.bz2)   tar xjf "$1"     ;;
            *.tar.gz)    tar xzf "$1"     ;;
            *.bz2)       bunzip2 "$1"     ;;
            *.rar)       unrar e "$1"     ;;
            *.gz)        gunzip "$1"      ;;
            *.tar)       tar xf "$1"      ;;
            *.tbz2)      tar xjf "$1"     ;;
            *.tgz)       tar xzf "$1"     ;;
            *.zip)       unzip "$1"       ;;
            *.Z)         uncompress "$1"  ;;
            *.7z)        7z x "$1"        ;;
            *)           echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# Make directory and cd into it
mkcd() {
    mkdir -p "$1" && cd "$1"
}

# ============================================================================
# Local Configuration
# ============================================================================

# Source local configuration if it exists
[ -f "$HOME/.zshrc.local" ] && source "$HOME/.zshrc.local"

# ============================================================================
# Completion and Key Bindings
# ============================================================================

# Enable better completion
autoload -Uz compinit && compinit

# Enable menu selection for completion
zstyle ':completion:*' menu select

# Case-insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Colored completion
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# Better history search
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward

# Move word by word
bindkey '^[[1;5C' forward-word
bindkey '^[[1;5D' backward-word

# ============================================================================
# Final Setup
# ============================================================================

# Set terminal title
case "$TERM" in
xterm*|rxvt*|alacritty)
    precmd () {print -Pn "\e]0;%n@%m: %~\a"}
    ;;
esac

# Welcome message
if command -v figlet &> /dev/null; then
    figlet -f small "Dev Ready!" 2>/dev/null || echo "=== Development Environment Ready ==="
else
    echo "=== Development Environment Ready ==="
fi

# Show versions of installed tools
echo "Installed Development Tools:"
command -v go &> /dev/null && echo "  • Go $(go version 2>/dev/null | awk '{print $3}')"
command -v erl &> /dev/null && echo "  • Erlang/OTP $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell 2>/dev/null | tr -d '\"')"
command -v node &> /dev/null && echo "  • Node.js $(node --version)"
command -v npm &> /dev/null && echo "  • npm $(npm --version)"
command -v docker &> /dev/null && echo "  • Docker $(docker --version 2>/dev/null | awk '{print $3}' | tr -d ',')"
echo ""

# Add cursor to PATH
export PATH="$HOME/.local/bin:$PATH"

# Generated for envman. Do not edit.
[ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"

alias capswap="setxkbmap -option ctrl:nocaps"
