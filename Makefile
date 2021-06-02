SWIPLSRC=/opt/src/swipl-devel
IMG=swipl-centos
IT=-it

MOUNTX11= -v /tmp/.X11-unix:/tmp/.X11-unix
UNIQID=$(shell date +%FT%T)

BUILDARGS=--build-arg CLEAN_BUILD=$$(cat .clean-build)  --build-arg INCREMENTAL_BUILD=$$(cat .incremental-build)

all::
	@echo "Targets:"
	@echo
	@echo "  image                Build the docker image"
	@echo "  update-clean         Prepare for clean build"
	@echo "  update-incremental   Prepare for incremental build"
	@echo

image:	Dockerfile .clean-build .incremental-build
	docker build $(BUILDARGS) -t $(IMG) . 2>&1 | tee mkimg.log

update-clean:
	echo $(UNIQID) > .clean-build
	echo $(UNIQID) > .incremental-build

update-incremental:
	echo $(UNIQID) > .incremental-build

.clean-build:
	echo $(UNIQID) > .clean-build
.incremental-build:
	echo $(UNIQID) > .incremental-build


run:
	docker run $(IT) $(IMG) /bin/bash


