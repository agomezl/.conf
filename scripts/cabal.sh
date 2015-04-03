#!/bin/bash

REAL_CABAL=/home/alien/.cabal/bin/cabal_real


if [ $1 == "install" ]
then
    echo "###############################"
    echo "### NOT IN A SANDBOX!! ########"
    echo "###############################"
    echo "Are you sure you want to install this package?"
    while true; do
        read -p "(y)es / (c)reate / (n)o: " ync
        case $ync in
            [Yy]* )
                break;;
            [Nn]* )
                exit 1;;
            [Cc]* )
                cabal sandbox init
                break
                ;;
            * ) echo "Please answer create, yes or no.";;
        esac
    done
fi

${REAL_CABAL} $@
