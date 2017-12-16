# Gets a working nice shell up and running

FROM fedora:25

# Basic stuff
RUN dnf -y install git zsh wget sudo

# Add user
RUN useradd -ms /bin/zsh agomezl && \
    echo "agomezl:docker" | chpasswd && \
    usermod -a -G wheel agomezl && \
    echo '%wheel ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers


USER agomezl
WORKDIR /home/agomezl

# .conf repo
ENV GIT_CONF "https://github.com/agomezl/.conf.git"
RUN git clone ${GIT_CONF} && \
 .conf/scripts/install.sh setup-rpmfusion && \
 .conf/scripts/install.sh dnf-base && \
 .conf/scripts/install.sh setup-ohzsh && \
 .conf/scripts/install.sh setup-conf && \
 .conf/scripts/install.sh setup-git && \
 .conf/scripts/install.sh setup-st && \
 .conf/scripts/install.sh setup-ls
