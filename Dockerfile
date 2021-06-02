FROM centos:7
LABEL maintainer "Jan Wielemaker <jan@swi-prolog.org>"

RUN dnf -y update && \
    dnf -y install cmake ninja-build libunwind gperftools-devel freetype-devel \
    gmp-devel java-1.8.0-openjdk-devel jpackage-utils libICE-devel \
    libjpeg-turbo-devel libSM-devel libX11-devel libXaw-devel libXext-devel \
    libXft-devel libXinerama-devel libXmu-devel libXpm-devel libXrender-devel \
    libXt-devel ncurses-devel openssl-devel pkgconfig readline-devel libedit-devel \
    unixODBC-devel zlib-devel uuid-devel libarchive-devel libyaml-devel

RUN	mkdir -p /opt/src &&
	cd /opt/src &&
	git clone https://github.com/SWI-Prolog/swipl-devel.gig &&
	cd swipl-devel &&
	git submodule update --init






