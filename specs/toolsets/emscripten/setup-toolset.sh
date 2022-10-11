#!/bin/bash
toolset_import_once python_3 || return 1
toolset_import_once nodejs || return 1
toolset_import_once xslt_tools || return 1

# toolset
export HAM_TOOLSET=EMSCRIPTEN
export HAM_TOOLSET_NAME=emscripten
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/emscripten"
export HAM_CPP_TOOLSET=$HAM_TOOLSET
export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

# path setup
case $HAM_OS in
    OSX*)
        if [ -z `which emcc` ]; then
            echo "W/Couldn't find emcc, will try to install it with brew."
            xcode-select --install
            ham-brew install emscripten
        fi
        export PATH=${HAM_TOOLSET_DIR}:${PATH}
        export BUILD_BIN_LOA=$HAM_BIN_LOA
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- emscripten ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`emcc --version | grep emcc`"
    if [ $? != 0 ]; then
        echo "E/Can't get version."
        return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
