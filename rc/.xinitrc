#!/bin/bash

bumblebeed --daemon &

xsetbg ~/Images/ShinobuHD.jpg

# Fire up apps
xsetroot -cursor_name ul_angle &
xscreensaver -no-splash &

#lxpanel &
trayer --edge top --align right --widthtype percent --width 10 --heighttype pixel --height 22 --transparent true --alpha 0 --tint 0x000000 --monitor primary &

batti &
if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi
/home/alien/.dropbox-dist/dropboxd &
gnome-sound-applet &


exec /home/alien/.cabal/bin/xmonad
