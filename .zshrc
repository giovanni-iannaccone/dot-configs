autoload -U compinit; compinit
autoload -Uz vcs_info

precmd() { vcs_info }

pwncheck() {
    file $1
    echo '\n'
    checksec --file=$1
}

shellcode() {
    gcc -nostdlib -static $1 -o shellcode.elf
    objcopy --dump-section .text=$2 shellcode.elf
    rm shellcode.elf
}

source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

zstyle ':vcs_info:git:*' formats '%b '
zstyle ':completion:*' menu select

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'

bindkey -e

setopt PROMPT_SUBST
PROMPT='%F{yellow}%*%f %F{blue}%~%f %F{green}${vcs_info_msg_0_}%f$ '

HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

export PATH=$PATH:/opt:~/go/bin

setopt inc_append_history

alias clearhist="echo > $HISTFILE"

alias ..="cd .."

alias cat="batcat"
alias catp="batcat -pp"

alias grep="rg"

alias emacs="emacs -nw"
alias ls="eza --icons"
alias la="eza --icons -a"
alias ll="eza --icons -l"
alias lla="eza --icons -la"
alias lt="eza --icons --tree"

alias github="eval '$(ssh-agent -s)' && ssh-add ~/.ssh/github && ssh -T git@github.com"
alias imgshow="kitty +kitten icat"
alias kssh="kitty +kitten ssh"

alias gdb="pwndbg"

fastfetch
