# SWI-Prolog continuous integration

This repository provides Docker files  for   multiple  Linux systems for
testing dependencies and configurations. Directories are organized as

    Distro/Version

The software in in `share`.   To run this locally:

  - Make sure you can run `docker`, normally by adding the user that
    will run the CI server to the `docker` group.

  - In share run this to get the web resources

        yarn

  - Start a Redis server at the default location (`localhost:6379`) or
    edit `config.yaml` to specify a host, port and optionally authentication.

  - Start the integration server using

        swipl share/ci_server.pl

  - Create an authorized user using

        htpasswd -d -c passwd username

  - Start the web server using

        swipl share/web_server.pl --port=8080 --no-fork

  - Surf to http://localhost:8080/ci/dev and login.  You should now see an empty
    build table.  Select an OS.  That should give build buttons.  Click the
    orange _Update OS and base image_.

## Installation as a service

For a more permanent installation the services can be activated as Linux
systemd services. The systemd scripts are   in  the `systemd` directory.
Other steps:

  - Create a user `ci` and add it to the `docker` group.
  - We assume a user `www-data`.  You may create another user without
    rights and update `ci/ciweb.service` accordingly.
  - Run `systemd/fix-permissions` as root to allow the `ci` user to create
    Docker files and write build logs.
  - Edit the scripts in `systemd` to fix locations and the HTTP port
  - Copy both scripts to `/etc/systemd/system`
  - Run

        sudo systemctl enable ci ciweb
        sudo systemctl start ci ciweb

## Update to new OS version

  - git mv OS/OldTag OS/NewTag
  - edit OS/NewTag/Dependencies.docker (FROM base)
