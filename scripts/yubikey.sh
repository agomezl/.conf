#!/bin/bash

# Requirements
sudo dnf -y install pam_yubico yubikey-manager
wget 'https://raw.githubusercontent.com/Yubico/yubikey-personalization/master/70-yubikey.rules'
sudo mv 70-yubikey.rules /etc/udev/rules.d/
sudo udevadm control --reload-rules && sudo udevadm trigger

# Challenge response files
sudo mkdir -p /var/yubico
sudo chown root.root /var/yubico
sudo chmod 700 /var/yubico

# Stored initial challenge and expected response in '$HOME/.yubico/challenge-123456'.
SERIAL=$(ykinfo -s | awk '{print $2}')
ykpamcfg -2 -v
sudo mv ~/.yubico/challenge-${SERIAL} /var/yubico/${USER}-${SERIAL}
sudo chown root.root /var/yubico/${USER}-${SERIAL}
sudo chmod 600 /var/yubico/${USER}-${SERIAL}

# Add this to files!!
# auth       sufficient   pam_yubico.so mode=challenge-response chalresp_path=/var/yubico
