#!/bin/bash

# toolset
export HAM_TOOLSET=THRIFT
export HAM_TOOLSET_NAME=thrift
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/thrift"

# path setup
case $HAM_OS in
    OSX*)
        ham-brew-install thrift "bin/thrift" || return 1
        PREFIX=`ham-brew-installdir thrift prefix`
        export THRIFT_BINDIR="${PREFIX}/opt/thrift"
        export THRIFT_INCDIR="${PREFIX}/include/thrift"
        export THRIFT_LIBDIR="${PREFIX}/lib"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

pathenv_add "$HAM_TOOLSET_DIR"

VER="--- thrift --------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  ham-check-file "$THRIFT_INCDIR/Thrift.h"
  errcheck $? $HAM_TOOLSET_NAME "Include check failed." || return 1
  ham-check-file "$THRIFT_LIBDIR/libthrift.a"
  errcheck $? $HAM_TOOLSET_NAME "Lib check failed." || return 1

  VER="$VER
`thrift --version`"
  errcheck $? $HAM_TOOLSET_NAME "Version check failed." || return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
