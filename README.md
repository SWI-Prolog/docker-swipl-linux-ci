# SWI-Prolog continuous integration

This repository provides Docker files  for   multiple  Linux systems for
testing dependencies and configurations. Directories are organized as

    Distro/Version

Each directory contains a Dockerfile and   a  Makefile. The Makefile has
the targets:

  - `make image` <br>
    Build all dependencies, SWI-Prolog and test it
  - `make run` <br>
    Start a shell in the image
  - `make update-clean` <br>
    Have the next `make image` update SWI-Prolog from git and rebuild in
    a clean directory
  - `make update-incremental` <br>
    Have the next `make image` update SWI-Prolog from git and perform an
    incremental build.
