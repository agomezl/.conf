#!/bin/bash

REAL_CABAL=/home/alien/.cabal/bin/cabal_real


if [ $1 == "install" ]
then
    while true; do
        echo "###############################"
        echo "### NOT IN A SANDBOX!! ########"
        echo "###############################"
        echo "Are you sure you want to install this package?"
        read -p "(y)es / (c)reate / (n)o: " ync
        case $ync in
            [Yy]* ) break;;
            [Nn]* ) exit 1;;
            [Cc]* ) cabal sandbox init;;
            * ) echo "Please answer create, yes or no.";;
        esac
    done
fi
