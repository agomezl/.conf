#!/bin/bash

exec > /home/agomezl/init.log 2>&1

sleep 1

PATH=$HOME/.cabal/bin:$HOME/.local/bin:$PATH

# Fire up apps
xsetroot -cursor_name ul_angle &

xcompmgr &

feh --bg-scale ~/Pictures/brown_mx.jpg &

stalonetray &

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

#proxy
export auto_proxy="file:/home/agomezl/.conf/scripts/proxy.pac"

volumeicon &

export GPG_TTY=$(tty)
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

emacsclient -a  --eval '(server-start)'

exec xmonad
