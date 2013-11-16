# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions

export PATH=$PATH:/home/$USER/.cabal/bin
export VISUAL='emacs -nw'

alias clean_emacs='find . -name "*~" -exec rm -rf {} \;'
alias clean_flymake='find . -name "dist" -exec rm -rf {} \;'
export PS1="<\[\e[0;31m\]\t\[\e[m\]//\[\e[0;33m\]\u\[\e[m\]//\[\e[0;31m\]\W\[\e[m\]> "
