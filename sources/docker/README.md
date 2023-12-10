# Overview

A set of base ham docker images.

These scripts build ham images based on the current master branch of ham. That
is the way ham is meant to be used, a bit like a rolling release Linux
distros.

Note that atm we intentionally don't build the images in CI since they dont
need to be updated very often as you should follow the practise of updating to
the latest ham in your dockerfile.

## Building an image on top of a ham docker image

Generally you'll want to base your image on the ham image that matches the
toolsets you want to use the closest. This will reduce the build time of your
images significantly. From there just update ham as the first step and then
import the toolsets you need afterward.

For example an hypothetical my-ham-nodejs-rust.dockerfile docker image:
```
FROM ham-base

# Update to the latest ham
WORKDIR $HAM_HOME
RUN bash -c "source $HAM_HOME/bin/ham-toolset repos > /dev/null && git-update ."
RUN git log -3 --oneline

# Install the nodejs & rust toolsets
RUN $HAM_HOME/bin/ham-toolset nodejs rust

CMD ["echo", "I/my-ham-nodejs-rust docker image."]
```

## Build and push

Build and push all:
```
./_docker_build_and_push.sh all
```
Note: Note that `./_docker_build_and_push.sh` always build ham-base with
nocache to ensure that its up-to-date.

Build a single image:
```
./_docker_build.sh latest ham-base
```

Rebuild a single image, needed to force a new git clone of the latest ham version:
```
./_docker_build.sh nocache latest ham-base
```

Push an already built image:
```
./_docker_push.sh latest ham-base
```

## Setup podman / login to docker

For podman:
```
sudo vi /etc/containers/registries.conf
# Append
[registries.search]
registries = ['docker.io']
```

Docker login:
```
docker login
```
