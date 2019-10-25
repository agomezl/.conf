#!/bin/sh

ALERT=/usr/share/sounds/freedesktop/stereo/complete.oga

$*
paplay ${ALERT}
