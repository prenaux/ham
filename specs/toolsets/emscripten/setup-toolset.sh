#!/bin/bash

toolset_import python_36
if [ $? != 0 ]; then return 1; fi
toolset_import nodejs
if [ $? != 0 ]; then return 1; fi
toolset_import xslt_tools
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=EMSCRIPTEN
export HAM_TOOLSET_VER=2_0
export HAM_TOOLSET_NAME=emscripten
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/emscripten"

# path setup
case $HAM_OS in
    OSX*)
        if [ -z `which emcc` ]; then
            echo "W/Couldn't find emcc, will try to install it with brew."
            xcode-select --install
            brew install emscripten
        fi
        export PATH=${HAM_TOOLSET_DIR}:${PATH}
        export BUILD_BIN_LOA=$HAM_BIN_LOA
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- emscripten ------------------------
`emcc --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
