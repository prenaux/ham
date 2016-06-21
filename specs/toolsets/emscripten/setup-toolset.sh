#!/bin/bash

toolset_import python_27
if [ $? != 0 ]; then return 1; fi
toolset_import nodejs
if [ $? != 0 ]; then return 1; fi
toolset_import xslt_tools
if [ $? != 0 ]; then return 1; fi
toolset_import cmake
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=EMSCRIPTEN
export HAM_TOOLSET_VER=1_25
export HAM_TOOLSET_NAME=emscripten
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/emscripten"

# Make the JSCC temporary folder
mkdir -p "$HOME/_ham/jscc/"

export EMSCRIPTEN_ROOT="${WORK}/emscripten"
export EMSCRIPTEN="${EMSCRIPTEN_ROOT}"
export NODE_JS="node"
export PYTHON="python"
export PATH="${EMSCRIPTEN_ROOT}":$PATH

# When not using a FASTCOMP enabled clang
# export EMCC_FAST_COMPILER=0

# path setup
case $HAM_OS in
    NT*)
        export LLVM_ROOT="${HAM_TOOLSET_DIR}/nt-x86/clang"
        if [ ! -e "$LLVM_ROOT/clang.exe" ]; then
            toolset_dl emscripten emscripten_nt-x86
            if [ ! -e "$LLVM_ROOT/clang.exe" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        export PATH="${LLVM_ROOT}":${PATH}
        ;;
    OSX*)
        export LLVM_ROOT="${HAM_TOOLSET_DIR}/osx-x64/clang/e1.25.0_64bit/"
        export TEMP="$HOME/_ham/emscripten/"
        mkdir -p $TEMP
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

if [ ! -e "$EMSCRIPTEN_ROOT/emcc" ]; then
    echo "Clone the emscripten repo into your WORK folder. (git clone https://github.com/prenaux/emscripten.git \"$WORK/emscripten\")"
    return 1
fi

EMSCRIPTEN_DEFAULT_DOT_FILE="${HAM_TOOLSET_DIR}/dot_emscripten"

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
