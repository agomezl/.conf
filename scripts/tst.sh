#!/bin/sh

export LD_LIBRARY_PATH=${HOME}/opt/libxft/src/.libs/
exec tabbed -f -r 2 st -w ''
