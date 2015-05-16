#!/bin/sh

module load ghc/7.8.3

exec /usr/bin/env emacsclient -c -a "" $*
