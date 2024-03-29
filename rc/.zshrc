# Emacs tramp support
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME=""

# ho-my-zsh plugins
plugins=(docker)

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

# Terminal title
export DISABLE_AUTO_TITLE="true"

# Set title
echo -ne "\033]0;${HOSTNAME}@${PWD##*/}\007"

function set-title() {
  touch /tmp/$$.title
  echo -ne "\033]0;${*}\007"
}

function unset-title() {
  rm -f /tmp/$$.title
}

function precmd () {
  # Do nothing if we are using an user title
  [ -f /tmp/$$.title ] && return

  TITLE="\033]0; ${HOSTNAME}@${PWD##*/}\007"
  echo -ne "$TITLE"
}

function preexec () {
  # Do nothing if we are using an user title
  [ -f /tmp/$$.title ] && return

  TITLE="\033]0; ${HOSTNAME}@${1%% *}\007"
  echo -ne "$TITLE"
}

# colorhighlight for less
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS=' -R '

# Modules
export MODULEPATH=/home/agomezl/.conf/modules

# dynamic libraries
export LD_LIBRARY_PATH=/home/agomezl/PhD/cake/polyml/lib:$LD_LIBRARY_PATH

# gpg-agent
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

# oh-my-zsh
source $ZSH/oh-my-zsh.sh

alias ls="lsd"
alias ll="lsd -lh"
alias lt="lsd --tree"
alias rbackup="rsync -zarvh"

alias ag="ag --pager less"
alias agf="ag -g"

alias dl="dirs -v"

function agg(){
    ag -G $2 $1
}

# Powerline
POWERLINE_FEDORA="/usr/share/powerline/zsh/powerline.zsh"
[ -f ${POWERLINE_FEDORA} ] && . ${POWERLINE_FEDORA}

POWERLINE_UBUNTU="/usr/share/powerline/bindings/zsh/powerline.zsh"
[ -f ${POWERLINE_UBUNTU} ] && . ${POWERLINE_UBUNTU}

[ -f "/home/agomezl/.ghcup/env" ] && source "/home/agomezl/.ghcup/env" # ghcup-env

export PATH=${HOME}/opt/GNAT/2021/bin:${PATH}

[ -f "/usr/share/doc/fzf/examples/key-bindings.zsh" ] && \
  source /usr/share/doc/fzf/examples/key-bindings.zsh

[ -f "/usr/share/doc/fzf/examples/completion.zsh" ] && \
  source /usr/share/doc/fzf/examples/completion.zsh

[ -f "/usr/share/fzf/shell/key-bindings.zsh" ] && \
  source /usr/share/fzf/shell/key-bindings.zsh

[ -f "/usr/share/fzf/shell/completion.zsh" ] && \
  source /usr/share/fzf/shell/completion.zsh

export FZF_DEFAULT_COMMAND='rg --files'

export FZF_CTRL_R_OPTS="--height 50% -1 --layout=reverse"

export FZF_CTRL_T_OPTS="--height 50% -1 --layout=reverse --multi --preview='[[ \$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always {} || cat {}) 2> /dev/null | head -300'"
export KALEIDOSCOPE_DIR=/home/agomezl/git/Kaleidoscope
