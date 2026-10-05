autoload -Uz compinit vcs_info
compinit

zstyle ':completion:*' menu select
zstyle ':vcs_info:git:*' formats '(%b) '

precmd() {
    vcs_info
}

setopt PROMPT_SUBST
PROMPT='%F{green}%n@%m%f %F{blue}%~%f %F{red}${vcs_info_msg_0_}%f$ '
PROMPT_EOL_MARK=''

setopt autocd
setopt inc_append_history
setopt hist_ignore_dups
setopt hist_ignore_space
setopt share_history

HISTFILE="$HOME/.zsh_history"
HISTSIZE=100000
SAVEHIST=100000

bindkey -e

if [[ -r /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh ]]; then
    source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
fi

if [[ -r /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
    source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'

bak() 		{ cp -- "$1" "$1.bak" }
restore() 	{ cp -- "$1.bak" "$1" }
rmbak() 	{ rm -- "$1.bak" }

brightness() {
    brightnessctl set "$1%"
}

github() {
    if [[ -z "$SSH_AUTH_SOCK" || ! -S "$SSH_AUTH_SOCK" ]]; then
        eval "$(ssh-agent -s)" >/dev/null
    fi

    ssh-add "$HOME/.ssh/github" || return 1
    ssh -T git@github.com
}

mkcd() {
    mkdir -p -- "$1" && cd -- "$1"
}

pwncheck() {
    if (( $# != 1 )); then
        echo "Usage: pwncheck <binary>" >&2
        return 1
    fi

    local file="$1"

    file -- "$file"
    echo
    checksec --file="$file"
    echo
    ldd -- "$file"
}

alias cat='batcat'
alias catp='batcat -pp'

alias ls='eza --icons'
alias la='eza --icons -a'
alias ll='eza --icons -l'
alias lla='eza --icons -la'
alias lt='eza --icons --tree'
alias lta='eza --icons --tree -a'

alias clearhist=': > "$HISTFILE"'
alias updatezsh='source "$HOME/.zshrc"'

alias copy='xclip -sel clip'

alias chmox='chmod +x'
alias rmcr='rm core.*'
alias rf='rm -rf'

alias emacs='emacs -nw'
alias make='make -j$(nproc)'
alias gdb='gdb -q'
alias objdump='objdump -M intel'

alias docker='podman'
alias curl='curl --path-as-is'

alias wgup='sudo wg-quick up'
alias wgdown='sudo wg-quick down'

alias venv='source "$HOME/Downloads/venv/bin/activate"'
alias webup='python3 -m http.server 8080'

alias angrinit='cp "$HOME/development/ctf/templates/angr-template.py" solve.py && venv'

alias -g NE='2>/dev/null'

path=(
    "$HOME/.local/bin"
    /opt
    "$HOME/go/bin"
    "$HOME/.local/share/gem/ruby/3.3.0/bin"
    /usr/sbin
    /sbin
    $path
)

export EDITOR='emacs'

if [[ -n "$EAT_SHELL_INTEGRATION_DIR" && \
      -r "$EAT_SHELL_INTEGRATION_DIR/zsh" ]]; then
    source "$EAT_SHELL_INTEGRATION_DIR/zsh"
    PROMPT="${PROMPT#0}"
fi
