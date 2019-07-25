#!/bin/bash

CONF_DIR=$(cd $(dirname ${BASH_SOURCE[0]} )/.. && pwd)

sudo dnf install zsh

if [ -h ~/.zshrc ]
then
    echo ".zshrc is already in place"
    exit 1
fi

sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

[ -h ~/.zshrc ] || rm -fr ~/.zshrc
ln -s ${CONF_DIR}/rc/.zshrc ~/.zshrc

[ -h ~/.config/powerline ] || rm -fr ~/.config/powerline
ln -s ${CONF_DIR}/rc/.config/powerline ~/.config/
