#!/bin/sh

unset CABAL_SANDBOX_CONFIG
unset CABAL_SANDBOX_PACKAGE_PATH
unset GHC_PACKAGE_PATH

#module load ghc/7.8.3

exec /usr/bin/env emacsclient -c -a "" $*
