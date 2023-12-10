#!/bin/bash

# toolset
export HAM_TOOLSET=DOCKER
export HAM_TOOLSET_NAME=docker
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/docker"

case "$HAM_BIN_LOA" in
  lin-x64)
    if [ -z "$(where_inpath docker)" ]; then
      ham-apt-get-install podman
    fi
    ;;

  *)
    complain docker_toolset "Unsupported arch '$HAM_BIN_LOA'."
    return 1
    ;;
esac
pathenv_add "$HAM_TOOLSET_DIR"

VER="--- docker ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(docker --version)"; then
    echo "E/Can't get version."
    return 1
  fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
