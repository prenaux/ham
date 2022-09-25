#!/bin/bash

# toolset
export HAM_TOOLSET=THRIFT
export HAM_TOOLSET_NAME=thrift
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/thrift"

# path setup
case $HAM_OS in
    LINUX*|OSX*)
        ham-brew-install thrift "bin/thrift" || return 1
        ;;
    LINUX*)
        if [ ! -e "`where_inpath thrift`" ]; then
          sudo apt-get -y install thrift-compiler
          if [ ! -e "`where_inpath thrift`" ]; then
            echo "E/Can't find thrift after thrift-compiler install."
            return 1
          fi
        fi
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
`thrift --version`"
  errcheck $? $HAM_TOOLSET_NAME "Version check failed." || return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
