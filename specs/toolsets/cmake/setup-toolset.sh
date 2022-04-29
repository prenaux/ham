#!/bin/bash

# toolset
export HAM_TOOLSET=CMAKE
export HAM_TOOLSET_NAME=cmake
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/cmake"
export HAM_CMAKE_HOME="${HAM_HOME}/toolsets/cmake"
export PATH="${HAM_TOOLSET_DIR}:${PATH}"

# path setup
case $HAM_OS in
    NT*)
        export CMAKE_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        pathenv_add "${CMAKE_DIR}/bin"
        if [ ! -e "$CMAKE_DIR/bin/cmake.exe" ]; then
            echo "E/cmake.exe doesn't exist in the toolset"
            return 1
        fi
        . ham-toolset-import.sh msvc_15_x64
        ;;
    OSX*)
        ham-brew-install cmake "bin/cmake"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

if ! VER="--- cmake ------------------------
$(cmake --version | grep "cmake version")"; then
  echo "E/Can't get version."
  return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
