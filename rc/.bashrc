# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# User specific aliases and functions

export PATH=/home/alien/.cabal/bin:$PATH
export VISUAL='emacs -nw'
export NAME="Alejandro Gómez Londoño"

alias clean_emacs='find . -name "*~" -exec rm -rf {} \;'
alias clean_flymake='find . -name "dist" -exec rm -rf {} \;'
alias clean_latex='rm *.{aux,log,nav,toc,vrb,out,snm}'
alias emacs='~/.conf/scripts/ec.sh'

export PS1="\n\[\033[1;37m\]\342\224\214\342\224\200[\[\033[1;34m\]\u@\h\
\[\033[1;37m\]]\342\224\200[\[\033[1;34m\]\t\[\033[1;37m\]]\[\033[1;37m\]\
\n\342\224\224\$([[ \$? != 0 ]] && echo \"\342\224\200(\[\033[1;34m\]\
\342\234\227\[\033[1;37m\])\")\342\224\200(\[\033[1;34m\]\W\[\033[1;37m\])\
>>=\[\033[0m\] "


if [[ "$TERM" == "dumb" ]]
then
  PS1='$ '
fi
