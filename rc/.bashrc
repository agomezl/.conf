# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# User specific aliases and functions

export PATH=$PATH:/home/alien/.cabal/bin
export VISUAL='emacs -nw'
export NAME="Alejandro Gómez Londoño"

alias clean_emacs='find . -name "*~" -exec rm -rf {} \;'
alias clean_flymake='find . -name "dist" -exec rm -rf {} \;'
alias clean_latex='rm *.{aux,log,nav,toc,vrb,out,snm}'
alias emacs='~/.conf/scripts/ec.sh'

export PS1="\n\[\033[1;37m\]\342\224\214\342\224\200<\[\033[1;31m\]\t\[\033[1;37m\]//\[\033[1;33m\]\u\[\033[1;37m\]//\[\033[1;31m\]\W\[\033[1;37m\]>\n\342\224\224\342\224\200>>=\[\033[0m\] "
