#!/bin/bash

# toolset
export HAM_TOOLSET=CMAKE
export HAM_TOOLSET_VER=35
export HAM_TOOLSET_NAME=cmake
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/cmake"

# path setup
case $HAM_OS in
    NT*)
        toolset_check_and_dl_ver cmake nt-x86 v1 || return 1
        export CMAKE_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH=${CMAKE_DIR}/bin:${PATH}
        if [ ! -e "$CMAKE_DIR/bin/cmake.exe" ]; then
            echo "E/cmake.exe doesn't exist in the toolset"
            return 1
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- cmake ------------------------
`cmake --version | grep "cmake version"`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
