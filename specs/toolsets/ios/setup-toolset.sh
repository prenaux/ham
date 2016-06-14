#!/bin/bash

toolset_import xslt_tools
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=IOS
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=ios
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/ios"

# path setup
case $HAM_OS in
    NT*)
        export IOS_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export IOSBUILDENV_PATH="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH="${HAM_TOOLSET_DIR}/":"${IOS_DIR}/Toolchain/":${PATH}
        if [ ! -e "$IOS_DIR" ] || [ -z "`type -P clang`" ]; then
            toolset_dl ios ios_nt-x86
            if [ ! -e "$IOS_DIR" ] || [ -z "`type -P clang`" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        # Default iOS arch
        export IOS_ARCH=arm64
        ;;
    OSX)
        # Default iOS arch
        export IOS_ARCH=arm64
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- ios ----------------------------
`clang --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
