#!/bin/bash
ln -s `pwd`/.emacs /home/$USER/.emac
cabal install ghc-mod

./emacs-pkg-install.sh flymake
./emacs-pkg-install.sh haskell-mode
./emacs-pkg-install.sh ghc
./emacs-pkg-install.sh ecb
