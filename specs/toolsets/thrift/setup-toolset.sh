#!/bin/bash

# toolset
export HAM_TOOLSET=THRIFT
export HAM_TOOLSET_NAME=thrift
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/thrift"

# path setup
case $HAM_OS in
  OSX*)
    ham-brew-install thrift "bin/thrift" || return 1
    ;;
  LINUX*)
    if [ ! -e "$(where_inpath thrift)" ]; then
      ham-apt-get-install thrift-compiler
      if [ ! -e "$(where_inpath thrift)" ]; then
        echo "E/Can't find thrift after thrift-compiler install."
        return 1
      fi
    fi
    ;;
  NT*)
    toolset_check_and_dl_ver thrift nt-x64 v18_1 || return 1
    pathenv_add "$HAM_TOOLSET_DIR/nt-x64"
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

pathenv_add "$HAM_TOOLSET_DIR"

VER="--- thrift --------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  VER="$VER
$(thrift --version)"
  errcheck $? $HAM_TOOLSET_NAME "Version check failed." || return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
