#!/bin/bash

# toolset
export HAM_TOOLSET=NODEJS
export HAM_TOOLSET_VER=081
export HAM_TOOLSET_NAME=nodejs_081
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/nodejs_081"

# path setup
case $HAM_OS in
    NT*)
        export NODEJS_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH="${NODEJS_DIR}":${PATH}
        if [ ! -e "$NODEJS_DIR" ]; then
            toolset_dl nodejs_081 nodejs_081_nt-x86
            if [ ! -e "$NODEJS_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- nodejs_081 ------------------------
`node --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
