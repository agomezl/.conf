#!/bin/bash

CONF_DIR=$(cd $(dirname ${BASH_SOURCE[0]} )/.. && pwd)

function setup-rpmfusion {
    sudo dnf -y install http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm \
                        http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
}

function dnf-base {
    sudo dnf -y install wget emacs gmp-devel \
         fail2ban aspell aspell-en aspell-es \
         unrar readline environment-modules \
         powerline the_silver_searcher colordiff \
         pdfgrep cabal-install ghc lxtask source-highlight \
         dmenu
}

function dnf-extras {
    sudo dnf -y install lm_sensors vlc NetworkManager-vpnc-gnome scrot \
         texlive-scheme-full system-config-printer cups # broadcom-wl
}

function dnf-gui {
    sudo dnf -y install feh volumeicon arandr libXrandr-devel libX11-devel \
         qbittorrent libXinerama-devel libXft-devel gpicview \
         evince network-manager-applet gnome-screensaver xcompmgr \
         xmonad
}

function dnf-groups {
    sudo dnf -y groupinstall "C Development Tools and Libraries" "Development Tools" "System Tools"
}

function all {
    setup-rpmfusion
    dnf-base
    dnf-groups
    dnf-extras
    dnf-gui
}

if [ -z "${1}" ]
then
    all
else
    ${1}
fi
