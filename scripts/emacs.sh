#!/bin/bash

CONF_DIR=$(cd $(dirname ${BASH_SOURCE[0]} )/.. && pwd)

sudo dnf -y install emacs

if [ -h ~/.emacs ]
then echo ".emacs is already in place"
     exit 1
fi

rm -fr ~/.emacs
ln -s ${CONF_DIR} ~/.emacs
