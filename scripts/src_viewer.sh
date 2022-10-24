#!/bin/sh

SRC_REPO=/home/s0001622/git/main-src

cd ${SRC_REPO}
git ls-tree -r HEAD --name-only | dmenu -i $@ | xargs -r st -c st-viewer bat --paging always
