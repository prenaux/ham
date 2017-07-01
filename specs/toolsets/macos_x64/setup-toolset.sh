#!/bin/bash

. ham-toolset-import.sh xslt_tools
if [ $? != 0 ]; then return 1; fi
toolset_import gcc_470
if [ $? != 0 ]; then return 1; fi

case $HAM_OS in
    OSX*)
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# toolset
export HAM_TOOLSET=CLANG
export HAM_TOOLSET_VER=33
export HAM_TOOLSET_NAME=clang_33
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}"

export RUN_DEBUGGER=lldb
export RUN_DEBUGGER_PARAMS=-f
export OSPLAT=X64
export BUILD_BIN_LOA=osx-x64

VER="--- macos_x64 -----------------------
`clang --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
