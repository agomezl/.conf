#!/bin/bash

if [ $UID -neq 0 ]
then
    echo "Run as root!! (use sudo)"
    exit
fi

su $USER
rm $HOME/.bashrc
ln -s `pwd`/rc/.bashrc $HOME/.bashrc
rm $HOME/.inputrc
ln -s `pwd`/rc/.inputrc $HOME/.inputrc
exit

function check
{
    if [ $? -eq 0 ]
    then
	echo "$1 [OK]" >> install.log
    else
	echo "$1 [ERROR]" >> install.log
	echo "Something goes wrong with $1"
	exit 1
}

yum install -y emacs git curl wget kernel-headers kernel-devel
yum localinstall --nogpgcheck http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-19.noarch.rpm
yum localinstall --nogpgcheck http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-19.noarch.rpm
yum -y groupinstall "Development Tools" "Development Libraries"

#Haskell
yum install -y gmp-devel gcc zlib-devel
ln -s /usr/lib64/libgmp.so /usr/lib64/libgmp.so.3
wget http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-x86_64-unknown-linux.tar.bz2
tar xvf ghc-7.6.3-x86_64-unknown-linux.tar.bz2
(
    cd ghc-7.6.3/
    ./configure
    make install
    check "Haskell"
)

#Haskell platform
yum install -y mesa-libGL-devel freeglut-devel mesa-libGLU-devel
wget http://www.haskell.org/platform/download/2013.2.0.0/haskell-platform-2013.2.0.0.tar.gz
tar xvf haskell-platform-2013.2.0.0.tar.gz
(
    cd haskell-platform-2013.2.0.0/
    sed '69d' scripts/build.sh > scripts/build.sh~
    mv scripts/build.sh~ scripts/build.sh
    chmod +x scripts/build.sh
    ./configure
    make install
    check "Haskell platform"
    su $USER
    cabal update
    cabal install cabal-install
    exit
    rm /usr/local/bin/cabal
    ln -s /home/alien/.cabal/bin/cabal /usr/local/bin/
)


#Emacs
yum install -y flex ruby rubygems rubygem-sqlite3.x86_64 mpfr-devel

#auto completado en C/C++#

#this link is to slow, try to find some copy of the file somewhere
wget http://cx4a.org/pub/gccsense/gcc-code-assist-0.1-4.4.4.tar.bz2
(
    tar xvjf gcc-code-assist-*.tar.bz2
    cd gcc-code-assist-*
    $ ./configure \
	--program-suffix=-code-assist \
	--enable-languages=c,c++ \
	--disable-bootstrap \
	--disable-multilib
    make # -j2
    make install
)
wget http://cx4a.org/pub/gccsense/gccsense-0.1.tar.bz2
(
    tar gccsense-0.1.tar.bz2
    cd gccsense-0.1
    cp bin/* /usr/local/bin/
    cp etc/gccsense.el $HOME/.emacs.d/
)

#haskell
(
    su $USER
    cd emacs
    ./install.sh
    exit
)

#latex

yum remove tex-* texlive-*
cat > /etc/yum.repos.d/texlive.repo <<EOF
[texlive]
name=texlive
baseurl=http://jnovy.fedorapeople.org/texlive/2012/packages.f17/
enabled=1
gpgcheck=0
EOF
yum update -y
yum install -y texlive-scheme-medium
yum install -y emacs-auctex
