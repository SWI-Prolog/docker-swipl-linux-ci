FROM centos:7
LABEL maintainer "Jan Wielemaker <jan@swi-prolog.org>"

RUN	yum -y update
RUN	yum -y group install "Development Tools"

# Requirements for cmake, download and install cmake
RUN	yum -y install wget openssl-devel
RUN	CMAKE_VERSION=3.20.3 && \
	mkdir -p /opt/src && \
	cd /opt/src && \
	wget https://github.com/Kitware/CMake/releases/download/v${CMAKE_VERSION}/cmake-${CMAKE_VERSION}.tar.gz && \
	tar xf cmake-${CMAKE_VERSION}.tar.gz && \
	cd cmake-${CMAKE_VERSION} && \
	./bootstrap --parallel=$(nproc) && gmake -j $(nproc) && gmake install

# SWI-Prolog dependencies
RUN	yum -y install git gperftools-devel freetype-devel \
	gmp-devel java-11-openjdk-devel jpackage-utils libICE-devel \
	libjpeg-turbo-devel libSM-devel libX11-devel libXaw-devel libXext-devel \
	libXft-devel libXinerama-devel libXmu-devel libXpm-devel libXrender-devel \
	libXt-devel ncurses-devel openssl-devel pkgconfig readline-devel libedit-devel \
	unixODBC-devel zlib-devel uuid-devel libarchive-devel libyaml-devel libdb-devel \
	openssl

# Download SWI-Prolog
RUN	mkdir -p /opt/src && \
	cd /opt/src && \
	git clone https://github.com/SWI-Prolog/swipl-devel.git && \
	cd swipl-devel && \
	git submodule update --init

ARG CLEAN_BUILD=unknown

# Update to the latest version
RUN	cd /opt/src/swipl-devel && \
	git pull && \
	git submodule update --init

RUN	cd /opt/src/swipl-devel && \
	rm -rf build  && \
	mkdir build  && \
	cd build  && \
	CFLAGS=-std=gnu99 cmake ..

RUN	cd /opt/src/swipl-devel/build  && \
	gmake -j $(nproc)

ARG INCREMENTAL_BUILD=unknown

RUN	cd /opt/src/swipl-devel && \
	git pull && \
	git submodule update --init

RUN	cd /opt/src/swipl-devel/build  && \
	cmake .. && \
	gmake -j $(nproc)

RUN	cd /opt/src/swipl-devel/build  && \
	LANG=en_US.utf8 ctest -j $(nproc) --output-on-failure

