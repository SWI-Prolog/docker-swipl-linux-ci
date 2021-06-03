FROM debian:10
LABEL maintainer "Jan Wielemaker <jan@swi-prolog.org>"

RUN	apt-get update

# SWI-Prolog dependencies
RUN	apt-get install -y git build-essential cmake ninja-build pkg-config \
        ncurses-dev libreadline-dev libedit-dev libgoogle-perftools-dev \
        libgmp-dev libssl-dev unixodbc-dev zlib1g-dev libarchive-dev \
        libossp-uuid-dev libxext-dev libice-dev libjpeg-dev libxinerama-dev \
	libxft-dev libxpm-dev libxt-dev \
        libdb-dev libpcre3-dev libyaml-dev default-jdk junit4

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
	LANG=C.UTF-8 ctest -j $(nproc) --output-on-failure

