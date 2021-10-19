#!/bin/bash

CONF_DIR=$(cd $(dirname ${BASH_SOURCE[0]} )/.. && pwd)

function setup-rpmfusion {
    sudo dnf -y install http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
}


#Other stuff
function dnf-base {
    sudo dnf -y install wget emacs gmp-devel \
         fail2ban aspell aspell-en aspell-es \
         unrar readline environment-modules \
         powerline the_silver_searcher colordiff \
         pdfgrep cabal-install ghc lxtask source-highlight \
         bat fd-find lsd
}

function dnf-extras {
    sudo dnf -y install lm_sensors vlc NetworkManager-vpnc-gnome scrot \
         texlive-scheme-full system-config-printer cups rlwrap code meld \
         ncdu
    # broadcom-wl
}

function dnf-gui {
    sudo dnf -y install feh volumeicon arandr libXrandr-devel libX11-devel \
         qbittorrent libXinerama-devel libXft-devel gpicview \
         evince network-manager-applet gnome-screensaver xcompmgr \
         xmonad
}

function setup-ohzsh {
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
}

function setup-conf {
    cd
    for file in $(ls -A ${CONF_DIR}/rc)
    do
        [ -f ${file} ] && mv ${file} ${file}_bkp || echo "${i}: No backup"
        [ -L ${file} ] && rm ${file}
        [ -f ${CONF_DIR}/rc/${file} ] && ln -s ${CONF_DIR}/rc/${file} .
    done

    mkdir -p ~/.local/bin
    [ -L ~/.local/bin/ec ] && rm ~/.local/bin/ec
    ln -s ${CONF_DIR}/scripts/ec.sh ~/.local/bin/ec

    rm -fr ~/.xmonad
    ln -s ${CONF_DIR}/rc/.xmonad ~/

    mkdir -p ~/.config
    rm -fr ~/.config/powerline
    ln -s ${CONF_DIR}/rc/.config/powerline ~/.config/
}

function setup-trayer {
    [ -d ~/Downloads ] || mkdir -p ~/Downloads
    cd Downloads
    wget 'ftp://ftp.pbone.net/mirror/ftp.afterstep.org/stable/rpms/RPMS/fc/18/x86_64/trayer-1.1.5-1.fc18.as.x86_64.rpm'
    sudo dnf install trayer-1.1.5-1.fc18.as.x86_64.rpm
}

function setup-desktop {
    sudo dnf remove awesome dwm i3 ratpoison cinnamon
    sudo cp /home/agomezl/.conf/scripts/xmonad.desktop /usr/share/xsessions/
}

function setup-dropbox {
    cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
    echo fs.inotify.max_user_watches=100000 | sudo tee -a /etc/sysctl.conf
    sudo sysctl -p
}

function setup-st {
    set -e
    sudo dnf -y install libX11-devel libXft-devel make
    [ -d ~/opt ] || mkdir -p ~/opt
    cd ~/opt
    [ -d st ] || git clone https://github.com/agomezl/st.git
    cd st
    mkdir -p ~/.fonts
    cp -r fonts/DejaVuSansMonoNerd/* ~/.fonts/
    cp -r fonts/DejaVuSansMono/* ~/.fonts/
    fc-cache -vf ~/.fonts/
    make clean install
    mkdir -p ~/.local/bin
    ln -s `pwd`/bin/st ~/.local/bin/
    ln -s ${CONF_DIR}/scripts/tst.sh ~/.local/bin/tst
}

function setup-ls {
    set -e

    sudo dnf -y install python libselinux-devel
    [ -d ~/opt ] || mkdir -p ~/opt
    cd ~/opt
    wget http://raw.githubusercontent.com/illinoisjackson/even-better-ls/master/ls_colors_generator.py
    chmod 755 ls_colors_generator.py
    cp ls_colors_generator.py ~/.local/bin/ls_colors_generator
    wget http://ftp.gnu.org/gnu/coreutils/coreutils-8.2.tar.xz
    tar -xf coreutils-8.2.tar.xz
    rm coreutils-8.2.tar.xz
    cd coreutils-8.2/src
    rm -rf ls.c
    wget http://raw.githubusercontent.com/illinoisjackson/even-better-ls/master/ls.c
    cd ..
    ./configure
    make
    cd src
    cp ls ~/.local/bin/ls-i
    cd ../..
    rm -fr coreutils-8.2/
}

function setup-git {
    git config --global user.email "alegomez544@gmail.com"
    git config --global user.name "Alejandro Gomez-Londono"
}

${1}
