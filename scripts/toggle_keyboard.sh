#!/bin/bash

DEVICE_ID=$(xinput list| \
            grep "AT Translated Set 2 keyboard" | \
            sed 's/^.*id=\([0-9]\+\).*$/\1/')

KB_STATE=$(xinput list-props ${DEVICE_ID} | grep "Device Enabled" | cut -d: -f2)

function toggle_keyboard {
    case ${KB_STATE} in
        *0)
            xinput enable ${DEVICE_ID}
            ;;
        *1)
            xinput disable ${DEVICE_ID}
            ;;
        *)
            echo "[Error] ${KB_STATE} unrecognized"
            ;;
    esac
}


case $1 in
    on)
        xinput enable ${DEVICE_ID}
        ;;
    off)
        xinput disable ${DEVICE_ID}
        ;;
    *)
        if [ -z "$1" ]
        then
            toggle_keyboard
        else
            echo "Usage: $0 on|of"
            exit 1
        fi
        ;;
esac
