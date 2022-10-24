#!/bin/bash

set -e

sudo apt-get update
sudo apt-get upgrade

# Install useful packges

sudo apt-get -y install emacs terminator zsh powerline dos2unix python3-pip bat htop silversearcher-ag

# Go to home
cd ~

# Useful folders
mkdir -p ~/.config
mkdir -p ~/.local/bin


# Setup ssh-keys on github and your servers
[ -f .ssh/id_rsa -a -f .ssh/id_rsa.pub ] || ssh-keygen

# Clone the configurations repo
[ -d .conf ] || git clone git@github.com:agomezl/.conf.git

# Install zsh
OH_ZSH_URL='https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh'
[ -d ~/.oh-my-zsh ] || sh -c "$(curl -fsSL ${OH_ZSH_URL})"

# Setup relevant conf files
for RC_FILE in '.bashrc' '.emacs' '.gitconfig' '.zshrc' 'htoprc' '.config/powerline' '.xmonad' '.xmobar'
do
  ln -sfn ~/.conf/rc/${RC_FILE} ~/${RC_FILE}
done

# bat
ln -sfn /usr/bin/batcat ~/.local/bin/bat

# Install lsd
LSD_URL='https://github.com/Peltoche/lsd/releases/download/0.20.1/lsd_0.20.1_amd64.deb'
LSD_FILE='lsd_0.20.1_amd64.deb'
[ -f ${LSD_FILE} ] || wget ${LSD_URL}
sudo dpkg -i lsd_0.20.1_amd64.deb

# powerline gitstatus
pip install powerline-gitstatus
