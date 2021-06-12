# Testing multiple configurations using Docker

## Base images

For each configuration, e.g., centos:7, create a docker image that contains

  - The build dependendencies for SWI-Prolog
  - A git checked-out version of the source
  - A build version of the source in build.<config>

named <distro>-swipl-ci:<tag>

## Test builds (from branch)

These run the base image using a a shell command that

  - Checks out the right version
  - For incremental build run the generator (make, ninja) in the
    build directory
  - For a clean build empty the build directory, configure and make.
  - Run the tests

## Per platform specs:

  - `Dependencies.docker`
  - YAML config file ci.yaml

## Branches

  - Remotes from which we can test are specified in `config.yaml` in the
    main dir.
