#!/bin/bash

# import dependencies
toolset_import repos
if [ $? != 0 ]; then return 1; fi
toolset_import clang_33
if [ $? != 0 ]; then return 1; fi
toolset_import nodejs_081
if [ $? != 0 ]; then return 1; fi
toolset_import java_jdk16
if [ $? != 0 ]; then return 1; fi
toolset_import python_27
if [ $? != 0 ]; then return 1; fi
toolset_import xslt_tools
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=EMSCRIPTEN
export HAM_TOOLSET_VER=2
export HAM_TOOLSET_NAME=emscripten
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/emscripten"

# emscripten setup
export EMSCRIPTEN_ROOT="${HAM_TOOLSET_DIR}/emscripten"
export EMSCRIPTEN="${EMSCRIPTEN_ROOT}"
export LLVM_ROOT="${CLANGDIR}"
export NODE_JS=
export PATH="${EMSCRIPTEN_ROOT}":$PATH

# Needed when not using a FASTCOMP enabled clang
export EMCC_FAST_COMPILER=0

EMSCRIPTEN_DEFAULT_DOT_FILE="${HAM_TOOLSET_DIR}/etc/.emscripten"

# Make the JSCC temporary folder
mkdir -p "$HOME/.ham/jscc/"

# dl if missing
if [ ! -e "$EMSCRIPTEN_ROOT" -o ! -e "$EMSCRIPTEN_DEFAULT_DOT_FILE" ]; then
    toolset_dl emscripten emscripten
    if [ ! -e "$EMSCRIPTEN_ROOT" -o ! -e "$EMSCRIPTEN_DEFAULT_DOT_FILE" ]; then
        echo "emscripten folder doesn't exist in the toolset"
        return 1
    fi
fi

# set JVM mem (need for big code base to build and for the JVM to not sometime
# fail to instantiate)
export _JAVA_OPTIONS="-Xms256m -Xmx768m"

# copy a configured .emscripten if needed
if [ ! -f "$HOME/.emscripten" -o ! -f "$HOME/.emscripten_sanity" ]; then
    echo "# Copying default .emscripten"
    cp -f "$EMSCRIPTEN_DEFAULT_DOT_FILE" "$HOME/.emscripten"
    emcc --version
fi

VER="--- emscripten ------------------------
`emcc --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
