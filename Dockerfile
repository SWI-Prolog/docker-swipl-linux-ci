FROM centos:7
LABEL maintainer "Jan Wielemaker <jan@swi-prolog.org>"

RUN yum -y update

RUN yum -y group install "Development Tools"

RUN yum -y install git ninja-build wget

RUN yum -y install openssl-devel

RUN	CMAKE_VERSION=3.20.3 && \
	mkdir -p /opt/src && \
	cd /opt/src && \
	wget https://github.com/Kitware/CMake/releases/download/v${CMAKE_VERSION}/cmake-${CMAKE_VERSION}.tar.gz && \
	tar xf cmake-${CMAKE_VERSION}.tar.gz && \
	cd cmake-${CMAKE_VERSION} && \
	./bootstrap --parallel=12 && gmake -j 12 && gmake install

RUN	yum -y install gperftools-devel freetype-devel \
	gmp-devel java-11-openjdk-devel jpackage-utils libICE-devel \
	libjpeg-turbo-devel libSM-devel libX11-devel libXaw-devel libXext-devel \
	libXft-devel libXinerama-devel libXmu-devel libXpm-devel libXrender-devel \
	libXt-devel ncurses-devel openssl-devel pkgconfig readline-devel libedit-devel \
	unixODBC-devel zlib-devel uuid-devel libarchive-devel libyaml-devel libdb

RUN	mkdir -p /opt/src && \
	cd /opt/src && \
	git clone https://github.com/SWI-Prolog/swipl-devel.git && \
	cd swipl-devel && \
	git submodule update --init

ARG BUILD_VER=unknown

RUN	cd /opt/src/swipl-devel && \
	rm -rf build  && \
	mkdir build  && \
	cd build  && \
	CFLAGS=-std=gnu99 cmake ..

RUN	cd /opt/src/swipl-devel/build  && \
	gmake -j $(nproc)

RUN	cd /opt/src/swipl-devel/build  && \
	LANG=en_US.utf8 ctest -j $(nproc) --output-on-failure

