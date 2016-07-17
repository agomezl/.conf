#!/bin/bash

echo fs.inotify.max_user_watches=100000 | sudo tee -a /etc/sysctl.conf; sudo sysctl -p

#sudo dnf install akmod-nvidia "kernel-devel-uname-r == $(uname -r)"
sudo dnf install wget emacs gmp-devel xcompmgr feh volumeicon \
     xscreensaver vlc NetworkManager-vpnc-gnome scrot fail2ban aspell \
     aspell-en aspell-es arandr broadcom-wl libXrandr-devel libX11-devel \
     lm_sensors unrar qbittorrent libXinerama-devel libXft-devel gpicview \
     evince readline-devel texlive-scheme-full system-config-printer.x86_64 \
     cups environment-modules libXpm-devel moreutils
