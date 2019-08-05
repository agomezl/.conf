#!/bin/bash

DEVICE_ID=$(xinput list| \
            grep Touchpad | \
            sed 's/^.*id=\([0-9]\+\).*$/\1/')

PROP_ID=$(xinput list-props ${DEVICE_ID} | \
          grep "Tapping Enabled ("  | \
          sed 's/.*(\([0-9]\+\)).*/\1/')


case $1 in
    on)
        xinput set-prop ${DEVICE_ID} ${PROP_ID} 1
        ;;
    off)
        xinput set-prop ${DEVICE_ID} ${PROP_ID} 0
        ;;
    *)
        echo "[Error] Usage $0 on|off"
        ;;
esac
