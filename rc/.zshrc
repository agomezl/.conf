# Emacs tramp support
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME=""

# ho-my-zsh plugins
plugins=(git docker)

# User configuration

bindkey '^[[1~' beginning-of-line
bindkey '^[[4~' end-of-line
bindkey '^[[2~' overwrite-mode
bindkey '^[[3~' delete-char

unset CABAL_SANDBOX_CONFIG
unset CABAL_SANDBOX_PACKAGE_PATH
unset GHC_PACKAGE_PATH

if [[ "$TERM" == "dumb" ]]
then
    PS1='$ '
    exit
fi

export PATH=$HOME/.local/bin:$HOME/.cabal/bin:/usr/local/bin:$PATH
# export MANPATH="/usr/local/man:$MANPATH"
export auto_proxy="file:/home/agomezl/.conf/scripts/proxy.pac"

# colorhighlight for less
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS=' -R '

# Modules
export MODULEPATH=/home/agomezl/.conf/modules

# gpg-agent
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

# oh-my-zsh
source $ZSH/oh-my-zsh.sh

# Even better ls
run_ls() {
    LS_COLORS=$(ls_colors_generator) ls-i --color=auto -w $(tput cols) "$@"
}

alias ls="run_ls"
alias ll="run_ls -lh"

alias ag="ag --pager less"

# Powerline
. /usr/share/powerline/zsh/powerline.zsh
