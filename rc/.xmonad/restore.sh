#!/bin/bash

if [ -z `xrandr --query | grep "HDMI1 connected"` ]
then
    xrandr --output DP2 --off --output DP1 --off --output HDMI2 --off --output HDMI1 --off --output LVDS1 --mode 1366x768 --pos 0x0 --rotate normal --output VGA1 --off
    xsetbg ~/Images/ShinobuHD.jpg
else
    xrandr --output DP2 --off --output DP1 --off \
        --output HDMI2 --off --output HDMI1 --mode 1920x1080 --pos 0x0 --rotate normal --primary \
        --output LVDS1 --off --output VGA1 --off
    xsetbg ~/Images/Shinobu2.jpg
fi


killall trayer
trayer --edge top --align right --widthtype percent --width 5 --heighttype pixel --height 22 --transparent true --alpha 0 --tint 0x000000 --monitor primary &
