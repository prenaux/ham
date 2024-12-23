#!/bin/bash

# toolset
export HAM_TOOLSET=GCC
export HAM_TOOLSET_NAME=gcc_470
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/gcc_470"
export HAM_CPP_TOOLSET=$HAM_TOOLSET
export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

# path setup
case $HAM_OS in
  OSX) ;;
  LINUX) ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

VER="--- gcc_470 ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(gcc --version)"; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
