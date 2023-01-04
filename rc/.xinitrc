#!/bin/bash

exec > ${HOME}/init.log 2>&1

sleep 1

export PATH=${HOME}/.cabal/bin:$HOME/.local/bin:$PATH
export PATH=${HOME}/opt/GNAT/2021/bin:$PATH


# Fire up apps
xsetroot -cursor_name ul_angle &

xcompmgr &

feh --bg-scale ~/Pictures/brown_mx.jpg &

# stalonetray &

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

#proxy
export auto_proxy="file:${HOME}/.conf/scripts/proxy.pac"

volumeicon &

export GPG_TTY=$(tty)
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

emacs --daemon
export LD_LIBRARY_PATH=${HOME}/opt/libxft/src/.libs/

exec xmonad
