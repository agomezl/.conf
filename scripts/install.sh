#!/bin/bash

#echo fs.inotify.max_user_watches=100000 | sudo tee -a /etc/sysctl.conf; sudo sysctl -p


function setup-rpmfusion {
    set -e
    su -c 'dnf install http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm'
    export S_FUSION=OK
}

#Nvidia driver
function setup-nvidia {
    set -e
    sudo dnf -y install akmod-nvidia "kernel-devel-uname-r == $(uname -r)"
    export S_NVIDIA=OK
}
#Other stuff
function setup-base {
    set -e
    sudo dnf -y install wget emacs gmp-devel xcompmgr feh volumeicon \
         xscreensaver vlc NetworkManager-vpnc-gnome scrot fail2ban aspell \
         aspell-en aspell-es arandr broadcom-wl libXrandr-devel libX11-devel \
         lm_sensors unrar qbittorrent libXinerama-devel libXft-devel gpicview \
         evince readline-devel texlive-scheme-full system-config-printer.x86_64 \
         cups environment-modules libXpm-devel powerline network-manager-applet
    export S_BASE=OK
}

#ghc
function setup-ghc {
    set -e
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
    export S_GHC=OK
}

function setup-cabal {
    set -e
    [ $S_GHC == "OK" ] || setup-ghc
    cd ~/Download
    wget 'https://www.haskell.org/cabal/release/cabal-install-1.22.9.0/cabal-install-1.22.9.0.tar.gz'
    tar xvfz cabal-install-1.22.9.0.tar.gz
    cd cabal-install-1.22.9.0
    ./bootstrap.sh
    cd
    export S_CABAL=OK
}

function setup-ohzsh {
    set -e
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    export S_OHZSH=OK
}

function setup-conf {
    set -e
    for file in $(ls -A .conf/rc)
    do
        [ -e $file ] && mv $file ${file}_bkp || echo "${i}: No backup"
        ln -s .conf/rc/$file $file
    done
    ln -s ~/.conf/scripts/ec.sh .local/bin/ec


    export S_CONF=OK
}

function setup-xmonad {
    set -e
    [ $S_CABAL == "OK" ] || setup-cabal
    cd ~/opt
    mkdir xmonad
    cd xmonad
    cabal update
    cabal sandbox init
    cabal install xmonad xmonad-contrib xmobar
    cd
    export S_XMONAD=OK
}


function setup-trayer {
    set -e
    [ -d ~/Downloads ] || mkdir ~/Downloads
    cd Downloads
    wget 'ftp://ftp.pbone.net/mirror/ftp.afterstep.org/stable/rpms/RPMS/fc/18/x86_64/trayer-1.1.5-1.fc18.as.x86_64.rpm'
    sudo dnf install trayer-1.1.5-1.fc18.as.x86_64.rpm
    cd
    export S_TRAYER=OK
}

function setup-desktop {
    set -e
    [ $S_XMONAD == " OK" ] || setup-xmonad
    sudo dnf remove awesome dwm i3 ratpoison cinnamon
    sudo cp ~/.conf/scripts/xmonad.desktop /usr/share/xsessions/
    export S_DESK=OK
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
    make clean install
    cd
    export S_ST=OK

}
