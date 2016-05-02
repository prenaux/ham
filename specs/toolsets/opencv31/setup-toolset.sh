#!/bin/bash

# toolset
export HAM_TOOLSET=OPENCV31
export HAM_TOOLSET_VER=31
export HAM_TOOLSET_NAME=opencv31
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/opencv31/"

# path setup
case $HAM_OS in
    NT*)
        export OPENCV31_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        if [ ! -e "${OPENCV31_DIR}x64/vc14/bin" ]; then
            toolset_dl opencv31 opencv31_nt-x86
            if [ ! -e "$OPENCV31_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        export OPENCV31_DIR_X64="${OPENCV31_DIR}/x64/vc14/"
        export OPENCV31_DIR_X86="${OPENCV31_DIR}/x86/vc15/"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# paths
export OPENCV31_DIR_INCLUDE="${OPENCV31_DIR}/include"

# compatible with OpenCV 3.0 'as-is'
export OPENCV30_DIR=${OPENCV30_DIR}
export OPENCV30_DIR_X64=${OPENCV31_DIR_X64}
export OPENCV30_DIR_X86=${OPENCV31_DIR_X86}
export OPENCV30_DIR_INCLUDE=${OPENCV31_DIR_INCLUDE}

VER="--- opencv31 ------------------------
OPENCV31_DIR_INCLUDE=${OPENCV31_DIR_INCLUDE}"
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
