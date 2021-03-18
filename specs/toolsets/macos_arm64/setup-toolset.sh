#!/bin/bash

. ham-toolset-import.sh xslt_tools
if [ $? != 0 ]; then return 1; fi
toolset_import gcc_470
if [ $? != 0 ]; then return 1; fi

case $HAM_OS in
    OSX*)
        # Added system sdk path using the xcode command line tool..
        export MACOS_SDK_PATH=$(xcrun --show-sdk-path)
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

export OSPLAT=ARM64
export BUILD_BIN_LOA=osx-arm64

# finding correct clang compiler dir
local dir=$(clang --version | grep InstalledDir)
export CMD_JSON_COMPILER_PATH=${dir#*' '}/

VER="--- macos_arm64 ---------------------
`clang --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
