#!/bin/bash

# toolset
export HAM_TOOLSET=OPENCV30
export HAM_TOOLSET_NAME=opencv30
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/opencv30/"

# path setup
case $HAM_OS in
    NT*)
        export OPENCV30_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        if [ ! -e "${OPENCV30_DIR}x64/vc12/bin" ]; then
            toolset_dl opencv30 opencv30_nt-x86
            if [ ! -e "$OPENCV30_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        export OPENCV30_DIR_X64="${OPENCV30_DIR}/x64/vc12/"
        export OPENCV30_DIR_X86="${OPENCV30_DIR}/x86/vc12/"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# paths
export OPENCV30_DIR_INCLUDE="${OPENCV30_DIR}/include"

VER="--- opencv30 ------------------------
OPENCV30_DIR_INCLUDE=${OPENCV30_DIR_INCLUDE}"
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
