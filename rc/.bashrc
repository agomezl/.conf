# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions

export PATH=$PATH:/home/alien/.cabal/bin
export VISUAL='emacs -nw'

alias clean_emacs='find . -name "*~" -exec rm -rf {} \;'
alias clean_flymake='find . -name "dist" -exec rm -rf {} \;'
alias emacs='~/.conf/scripts/ec.sh'
#export PS1="<\[\e[0;31m\]\t\[\e[m\]//\[\e[0;33m\]\u\[\e[m\]//\[\e[0;31m\]\W\[\e[m\]> "

export PS1="\n\[\033[1;37m\]\342\224\214\342\224\200<\[\033[1;31m\]\t\[\033[1;37m\]//\[\033[1;33m\]\u\[\033[1;37m\]//\[\033[1;31m\]\W\[\033[1;37m\]>\n\342\224\224\342\224\200>>=\[\033[0m\] "

alias shutdown='\
echo "##################### WARNING ##########################";\
echo "# You are trying to shutdown the master node of Apolo, #";\
echo "# are you sure of what you are doing?                  #";\
echo "# if you are, you can use /sbin/shutdown               #";\
echo "########################################################";\
sleep 15;\
echo "you know what?";\
sleep 5;\
echo "Think about your sins";\
exit'
