#!/bin/bash

function pre_hook {

    /home/agomezl/.conf/scripts/toggle_keyboard.sh on
}

function post_hook {

    true
}

case $1 in
    pre)
        pre_hook
        ;;
    post)
        post_hook
        ;;
    *)
        echo "Usage $0 pre|post"
esac
