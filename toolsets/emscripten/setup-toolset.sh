#!/bin/bash

# import dependencies
toolset_import xslt_tools
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=EMSCRIPTEN
export HAM_TOOLSET_VER=3
export HAM_TOOLSET_NAME=emscripten
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/emscripten

# path setup
case $HAM_OS in
    OSX)
        export PATH=${HAM_TOOLSET_DIR}/osx:${PATH}
        pushd ${HAM_TOOLSET_DIR}/osx >> /dev/null
        . emsdk_add_path
        popd >> /dev/null
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
