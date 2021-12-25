#!/bin/bash

# import
case $HAM_OS in
    NT*)
        ;;
    OSX*)
        . ham-toolset-import.sh wine
        if [ $? != 0 ]; then return 1; fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# toolset
export HAM_TOOLSET=NICGC
export HAM_TOOLSET_NAME=nicgc
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/nicgc"

# path setup
case $HAM_OS in
    NT*)
        ;;
    OSX*)
        toolset_check_and_dl_ver nicgc osx-x64 v1 || return 1
        export NICGC_DIR="${HAM_TOOLSET_DIR}/osx-x64"
        export PATH="${NICGC_DIR}/bin":"${NICGC_DIR}/bin":${PATH}
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# path
export PATH="${HAM_TOOLSET_DIR}":${PATH}

# version
VER="--- nicgc ------------------------
`nicgc -v 2>&1`"
if [ $? != 0 ]; then
    echo "E/Can't get nicgc version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
