#!/bin/bash

#echo fs.inotify.max_user_watches=100000 | sudo tee -a /etc/sysctl.conf; sudo sysctl -p


function setup-rpmfusion {
    su -c 'dnf install http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm'
}

#Nvidia driver
function setup-nvidia {
    sudo dnf -y install akmod-nvidia "kernel-devel-uname-r == $(uname -r)"
}

#Other stuff
function setup-base {
    set -e
    sudo dnf -y install wget emacs gmp-devel xcompmgr feh volumeicon \
         xscreensaver vlc NetworkManager-vpnc-gnome scrot fail2ban aspell \
         aspell-en aspell-es arandr broadcom-wl libXrandr-devel libX11-devel \
         lm_sensors unrar qbittorrent libXinerama-devel libXft-devel gpicview \
         evince readline-devel texlive-scheme-full system-config-printer.x86_64 \
         cups environment-modules libXpm-devel powerline network-manager-applet \
         the_silver_searcher colordiff pdfgrep
}

#ghc
function setup-ghc {
    [ -d ~/Downloads ] || mkdir ~/Downloads
    sudo ln -s /usr/lib64/libgmp.so /usr/lib64/libgmp.so.3
    cd ~/Downloads
    wget 'http://downloads.haskell.org/~ghc/7.10.3/ghc-7.10.3-x86_64-centos67-linux.tar.bz2'
    tar xvf ghc-7.10.3-x86_64-centos67-linux.tar.bz2
    cd ghc-7.10.3
    [ -d ~/opt ] || mkdir ~/opt
    ./configure --prefix=~/opt/ghc-7.10.3
    make install
    cd
}

function setup-cabal {
    cd ~/Download
    wget 'https://www.haskell.org/cabal/release/cabal-install-1.22.9.0/cabal-install-1.22.9.0.tar.gz'
    tar xvfz cabal-install-1.22.9.0.tar.gz
    cd cabal-install-1.22.9.0
    ./bootstrap.sh
    cd
}

function setup-ohzsh {
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
}

function setup-conf {
    for file in $(ls -A .conf/rc)
    do
        [ -e $file ] && mv $file ${file}_bkp || echo "${i}: No backup"
        ln -s .conf/rc/$file $file
    done
    ln -s ~/.conf/scripts/ec.sh .local/bin/ec

}

function setup-xmonad {
    cd ~/opt
    mkdir xmonad
    cd xmonad
    cabal update
    cabal sandbox init
    cabal install xmonad xmonad-contrib xmobar
    cd
}


function setup-trayer {
    set -e
    [ -d ~/Downloads ] || mkdir ~/Downloads
    cd Downloads
    wget 'ftp://ftp.pbone.net/mirror/ftp.afterstep.org/stable/rpms/RPMS/fc/18/x86_64/trayer-1.1.5-1.fc18.as.x86_64.rpm'
    sudo dnf install trayer-1.1.5-1.fc18.as.x86_64.rpm
    cd
}

function setup-desktop {
    set -e
    sudo dnf remove awesome dwm i3 ratpoison cinnamon
    sudo cp ~/.conf/scripts/xmonad.desktop /usr/share/xsessions/
}

function setup-dropbox {
    cd ~ && wget -O - "https://www.dropbox.com/download?plat=lnx.x86_64" | tar xzf -
}

function setup-st {
    set -e

    [ -d ~/opt ] || mkdir ~/opt
    cd opt
    git clone https://github.com/agomezl/st.git
    cd st
    mkdir -p ~/.fonts
    cp -r fonts/DejaVuSansMono/ ~/.fonts/
    fc-cache -vf ~/.fonts/
    make clean install
    cd

}
