#!/bin/bash

# toolset
export HAM_TOOLSET=NGINX
export HAM_TOOLSET_NAME=nginx
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/nginx"

# path setup
case $HAM_OS in
  OSX*)
    ham-brew-install nginx "bin/nginx"
    ;;
  LINUX*)
    if [ -z "$(where_inpath nginx)" ]; then
      sudo apt -y update
      sudo apt install -y nginx
    fi
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

VER="--- nginx ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(nginx -v 2>&1)"; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
