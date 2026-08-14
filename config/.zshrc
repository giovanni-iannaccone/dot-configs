autoload -U compinit; compinit
autoload -Uz vcs_info

precmd() { vcs_info }

brightness() {
    brightnessctl set "$1%"
}

pwncheck() {
    file $1
    echo '\n'
    checksec --file=$1
    echo '\n'
    ldd $1
}

sc32() {
    nasm -f elf -o shellcode.elf $1
    objcopy --dump-section .text=shellcode shellcode.elf
    rm shellcode.elf
}

sc64() {
    gcc -nostdlib -static $1 -o shellcode.elf
    objcopy --dump-section .text=shellcode shellcode.elf
    rm shellcode.elf
}

source /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

zstyle ':vcs_info:git:*' formats '%b '
zstyle ':completion:*' menu select

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'

bindkey -e

setopt PROMPT_SUBST
PROMPT='%F{green}%*%f %F{blue}%~%f %F{red}${vcs_info_msg_0_}%f$ '

HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

setopt inc_append_history

alias clearhist="echo > $HISTFILE"

alias ..="cd .."
alias grep="rg"

alias cat="batcat"
alias catp="batcat -pp"

alias chmox="chmod +x"

alias rmcr="rm core.*"
alias rf="rm -rf"

alias emacs="emacs -nw"

alias ls="eza --icons"
alias la="eza --icons -a"
alias ll="eza --icons -l"
alias lla="eza --icons -la"
alias lt="eza --icons --tree"

alias angrinit="cp ~/development/ctf/templates/angr-template.py solve.py; venv"
alias gdb="gdb -q"
alias objdump="objdump -M intel"

alias github="eval '$(ssh-agent -s)' && ssh-add ~/.ssh/github && ssh -T git@github.com"
alias curl="curl --path-as-is"

alias venv="source ~/Downloads/venv/bin/activate"
alias webup="python3 -m http.server 8080"

alias wgdown="sudo wg-quick down "
alias wgup="sudo wg-quick up "

alias updatezsh="source ~/.zshrc"

alias -g NE="2>/dev/null"

export PATH=$PATH:~/.local/bin/:/opt:~/go/bin:/home/giovanni/.local/share/gem/ruby/3.3.0/bin:/usr/sbin:/sbin:~/.cargo/bin
export EDITOR=emacs
