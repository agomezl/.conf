#!/bin/bash

DEVICE_ID=$(xinput list| \
            grep "AT Translated Set 2 keyboard" | \
            sed 's/^.*id=\([0-9]\+\).*$/\1/')

KB_STATE=$(xinput list-props ${DEVICE_ID} | grep "Device Enabled" | cut -d: -f2)


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
