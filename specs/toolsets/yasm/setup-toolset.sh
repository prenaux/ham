#!/bin/bash

# toolset
export HAM_TOOLSET=YASM
export HAM_TOOLSET_NAME=yasm
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/yasm"

# path setup
case $HAM_OS in
    NT*)
        export YASM_BIN_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH="${YASM_BIN_DIR}":${PATH}
        if [ ! -e "$YASM_BIN_DIR" ]; then
            toolset_dl yasm yasm_nt-x86
            if [ ! -e "$YASM_BIN_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    OSX*)
        export YASM_BIN_DIR="${HAM_TOOLSET_DIR}/osx-x86/"
        export PATH="${YASM_BIN_DIR}":${PATH}
        if [ ! -e "$YASM_BIN_DIR/yasm" ]; then
            toolset_dl yasm yasm_osx-x86
            if [ ! -e "$YASM_BIN_DIR/yasm" ]; then
                echo "E/osx-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# path
export PATH="${HAM_TOOLSET_DIR}":${PATH}

# version
VER="--- yasm ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`yasm --version | grep '^yasm'`"
    if [ $? != 0 ]; then
        echo "E/Can't get yasm version."
        return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
