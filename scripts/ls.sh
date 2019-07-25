#!/bin/bash

CONF_DIR=$(cd $(dirname ${BASH_SOURCE[0]} )/.. && pwd)

sudo dnf -y install python libselinux-devel
[ -d ~/opt ]        || mkdir -p ~/opt
[ -d ~/.local/bin ] || mkdir -p ~/.local/bin

cd ~/opt
wget 'http://raw.githubusercontent.com/illinoisjackson/even-better-ls/master/ls_colors_generator.py'
chmod 755 ls_colors_generator.py
cp ls_colors_generator.py ~/.local/bin/ls_colors_generator

wget http://ftp.gnu.org/gnu/coreutils/coreutils-8.29.tar.xz
tar -xf coreutils-8.29.tar.xz
rm coreutils-8.29.tar.xz
cd coreutils-8.29/src
rm -rf ls.c
wget http://raw.githubusercontent.com/illinoisjackson/even-better-ls/master/ls.c
cd ..
./configure
make

cd src
cp ls ~/.local/bin/ls-i
cd ../..
# rm -fr coreutils-8.2/
