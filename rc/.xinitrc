#!/bin/bash

exec > /home/agomezl/init.log 2>&1

PATH=$HOME/.cabal/bin:$HOME/.local/bin:$PATH
# Fire up apps
xsetroot -cursor_name ul_angle &
xscreensaver -no-splash &

xcompmgr &

#~/.screenlayout/Dual.sh

feh --bg-scale ~/Images/evangelion.jpg

trayer --edge top --align right --widthtype percent --width 10 --heighttype pixel --height 22 --transparent true --alpha 0 --tint 0x000000 --monitor primary &

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

#proxy
export auto_proxy="file:/home/agomezl/.conf/scripts/proxy.pac"

volumeicon &
~/.dropbox-dist/dropboxd &

XMONAD_SANDBOX=/home/agomezl/opt/xmonad/cabal.sandbox.config
cabal --sandbox-config-file=${XMONAD_SANDBOX} exec xmonad
