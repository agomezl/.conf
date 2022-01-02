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
         bat fd-find lsd xsetroot feh stalonetray volumeicon \
         htop vim pip xinput
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

function setup-desktop {
    sudo dnf remove awesome dwm i3 ratpoison cinnamon
    sudo cp /home/agomezl/.conf/scripts/xmonad.desktop /usr/share/xsessions/
}

function setup-git {
    git config --global user.email "agomezl@pm.me"
    git config --global user.name "Alejandro Gómez-Londoño"
}

${1}
