#!/bin/bash

# Fire up apps
xsetroot -cursor_name ul_angle &
xscreensaver -no-splash &

xcompmgr &

~/.screenlayout/Dual.sh

feh --bg-scale ~/Images/evangelion.png

trayer --edge top --align right --widthtype percent --width 10 --heighttype pixel --height 22 --transparent true --alpha 0 --tint 0x000000 --monitor primary &

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi


volumeicon &
~/.dropbox-dist/dropboxd &

xmonad
