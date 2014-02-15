#!/bin/bash

xrandr --query | grep -q "HDMI1 connected"
if [ $? -eq 0 ]
then
    xrandr --query | grep -q "DP2 connected"
    if [ $? -eq 0 ]
    then
	xrandr --output DP1 --off \
	    --output HDMI2 --off \
	    --output HDMI1 --mode 1920x1080 --pos 0x0 --rotate normal --primary \
	    --output DP2 --mode 1280x1024 --pos 1920x56 --rotate normal \
	    --output LVDS1 --off --output VGA1 --off
	xsetbg -at 1920,56 ~/Images/hanekawa.jpg ~/Images/Shinobu2.jpg
	killall trayer
	trayer --edge top --align right --widthtype percent --width 10 --heighttype pixel --height 22 --transparent true --alpha 0 --tint 0x000000 --monitor primary &
	exit 0
    fi
    xrandr --output DP2 --off \
	--output DP1 --off \
	--output HDMI2 --off \
	--output HDMI1 --mode 1920x1080 --pos 0x0 --rotate normal --primary \
	--output LVDS1 --mode 1366x768 --pos 1920x312 --rotate normal \
	--output VGA1 --off
    xsetbg -at 1920,312 ~/Images/ShinobuHD.jpg ~/Images/Shinobu2.jpg
    killall trayer
    trayer --edge top --align right --widthtype percent --width 10 --heighttype pixel --height 22 --transparent true --alpha 0 --tint 0x000000 --monitor primary &
    exit 0
fi

xrandr --query | grep -q "DP2 connected"
if [ $? -eq 0 ]
then
    xrandr --output HDMI1 --off \
	--output DP1 --off \
	--output HDMI2 --off \
	--output DP2 --mode 1920x1080 --pos 0x0 --rotate normal --primary \
	--output LVDS1 --mode 1366x768 --pos 1920x312 --rotate normal \
	--output VGA1 --off
    xsetbg -at 1920,312 ~/Images/ShinobuHD.jpg ~/Images/Shinobu2.jpg
    killall trayer
    trayer --edge top --align right --widthtype percent --width 10 --heighttype pixel --height 22 --transparent true --alpha 0 --tint 0x000000 --monitor primary &
    exit 0
fi

# xrandr --output DP2 --off --output DP1 --off \
#     --output HDMI2 --off --output HDMI1 --mode 1920x1080 --pos 0x0 --rotate normal --primary \
#     --output LVDS1 --off --output VGA1 --off
# xsetbg ~/Images/Shinobu2.jpg
# killall trayer
# trayer --edge top --align right --widthtype percent --width 10 --heighttype pixel --height 22 --transparent true --alpha 0 --tint 0x000000 --monitor primary &
