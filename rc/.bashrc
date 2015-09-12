# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# User specific aliases and functions

shopt -s histappend
export HISTCONTROL=ignoredups:erasedups

# export PATH=/home/agomezl/.cabal/bin:$PATH
export VISUAL='emacs -nw'
export NAME="Alejandro Gómez Londoño"
export MODULEPATH=~/.local/modules

alias clean_emacs='find . -name "*~" -exec rm -rf {} \;'
alias clean_flymake='find . -name "dist" -exec rm -rf {} \;'
alias clean_latex='rm *.{aux,log,nav,toc,vrb,out,snm}'
alias emacs='~/.conf/scripts/ec.sh'

unset CABAL_SANDBOX_CONFIG
unset CABAL_SANDBOX_PACKAGE_PATH
unset GHC_PACKAGE_PATH

powerline_path=$(python -c 'import pkgutil; print pkgutil.get_loader("powerline").filename' 2>/dev/null)
if [[ "$powerline_path" != "" ]]   && \
   [ -f `which powerline-daemon` ] && \
   ! [[ "$TERM" == "dumb" ]]
then
    powerline-daemon -q
    POWERLINE_BASH_CONTINUATION=1
    POWERLINE_BASH_SELECT=1
  . /usr/share/powerline/bash/powerline.sh
else
    export PS1="\n\[\033[1;37m\]\342\224\214\342\224\200[\[\033[1;32m\]\u@\h\
    \[\033[1;37m\]]\342\224\200[\[\033[1;32m\]\t\[\033[1;37m\]]\[\033[1;37m\]\
    \n\342\224\224\$([[ \$? != 0 ]] && echo \"\342\224\200(\[\033[1;32m\]\
    \342\234\227\[\033[1;37m\])\")\342\224\200(\[\033[1;32m\]\W\[\033[1;37m\])\
    >>=\[\033[0m\] "
fi

if [[ "$TERM" == "dumb" ]]
then
  PS1='$ '
fi
