# Testing multiple configurations using Docker

## Base images

For each configuration, e.g., centos:7, create a docker image that contains

  - The build dependendencies for SWI-Prolog
  - A git checked-out version of the source
  - A build version of the source in build.<config>

named <distro>-swipl-ci:<tag>

## Incremental build (from branch)

    FROM <distro>-swipl-ci:<tag>
    RUN <update source to branch>
    WORKING_DIRECTORY /opt/src/swipl-devel/build.<config>
    RUN ninja/make
    RUN ctest

## Clean build (from branch)

    FROM <distro>-swipl-ci:<tag>
    RUN <update source to branch>
    WORKING_DIRECTORY /opt/src/swipl-devel
    RUN mkdir build.<config>.clean && cmake ...
    RUN ninja/make
    RUN ctest

## Per platform specs:

  - Docker statement(s) to get build dependencies
  - Test environment (locale)
  - Platform CMake options
    - CFLAGS=
    - Select gcc/clang/...
    - Skip specific dependencies
    - *_ROOT, etc to get the right dependencies (hardly ever needed)
  - Platform build tool (make, ninja)

## Config

  - Config CMake options
    - PGO, Single threaded, Packages

# Prolog based rules?
