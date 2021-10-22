#!/bin/bash

TASKW_DIR=~/.task
TIMEW_DIR=~/.timewarrior

set -e
# Sync taskwarrior
cd ${TASKW_DIR}
if [[ `git status --porcelain` ]]
then
    git add -u
    git commit -m "$(date)"
    git push
else
    git pull
fi

cd ${TIMEW_DIR}
if [[ `git status --porcelain` ]]
then
    git add -u
    git commit -m "$(date)"
    git push
else
    git pull
fi
