#!/bin/bash

CONF_DIR=$(cd $(dirname ${BASH_SOURCE[0]} )/.. && pwd)

sudo dnf install xmonad xmobar

if [ -h ~/.xmonad ]
then
    echo ".xmonad is already in place"
    exit 1
fi


[ -h ~/.xmonad ] || rm -fr .xmonad
ln -s ${CONF_DIR}/rc/.xmonad ~/

sudo cp ${CONF_DIR}/scripts/xmonad.desktop /usr/share/xsessions/

[ -h ~/.xinitrc ] || rm -fr ~/.xinitrc
ln -s ${CONF_DIR}/rc/.xinitrc ~/

[ -h ~/.xmobar ] || rm -fr ~/.xmobar
ln -s ${CONF_DIR}/rc/.xmobarrc ~/
