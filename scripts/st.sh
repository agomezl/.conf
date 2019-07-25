#!/bin/bash

sudo dnf -y install libX11-devel libXft-devel make

[ -d ~/opt ] || mkdir -p ~/opt
cd ~/opt

# Repo
[ -d st ] || git clone https://github.com/agomezl/st.git
cd st

# Fonts
mkdir -p ~/.fonts
cp -r fonts/DejaVuSansMonoNerd/* ~/.fonts/
cp -r fonts/DejaVuSansMono/* ~/.fonts/
fc-cache -vf ~/.fonts/

# Build
make clean install
mkdir -p ~/.local/bin
ln -s `pwd`/bin/st ~/.local/bin/
