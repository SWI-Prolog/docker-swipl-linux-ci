FROM fedora:34
LABEL maintainer "Jan Wielemaker <jan@swi-prolog.org>"

RUN	dnf -y update

# SWI-Prolog dependencies
RUN	dnf -y install git cmake ninja-build gcc gcc-c++ \
	gperftools-devel freetype-devel gmp-devel java-11-openjdk-devel \
	jpackage-utils libICE-devel libjpeg-turbo-devel libSM-devel \
	libX11-devel libXaw-devel libXext-devel libXft-devel \
	libXinerama-devel libXmu-devel libXpm-devel libXrender-devel \
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
	cmake -G Ninja ..

RUN	cd /opt/src/swipl-devel/build  && \
	ninja

ARG INCREMENTAL_BUILD=unknown

RUN	cd /opt/src/swipl-devel && \
	git pull && \
	git submodule update --init

RUN	cd /opt/src/swipl-devel/build  && \
	cmake .. && \
	ninja

RUN	cd /opt/src/swipl-devel/build  && \
	LANG=en_US.utf8 ctest -j $(nproc) --output-on-failure

