autoload -U compinit; compinit
autoload -Uz vcs_info
precmd() { vcs_info }

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

export PATH=$PATH:~/go/bin

setopt inc_append_history
alias clearhst="echo > $HISTFILE"
alias ls="ls --color=auto"
