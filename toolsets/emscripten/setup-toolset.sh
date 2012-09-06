#!/bin/bash

# import dependencies
toolset_import gcc_470
if [ $? != 0 ]; then return 1; fi
toolset_import clang_31
if [ $? != 0 ]; then return 1; fi
toolset_import nodejs_081
if [ $? != 0 ]; then return 1; fi
toolset_import java_jre6
if [ $? != 0 ]; then return 1; fi
toolset_import python_26
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=EMSCRIPTEN
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=emscripten
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/emscripten

# emscripten setup
export EMSCRIPTEN_ROOT=${HAM_TOOLSET_DIR}/emscripten
export LLVM_ROOT=${CLANGDIR}
export NODE_JS=
export PATH=${EMSCRIPTEN_ROOT}:$PATH

# set JVM mem (need for big code base to build and for the JVM to not sometime
# fail to instantiate)
export _JAVA_OPTIONS="-Xms256m -Xmx768m"

# copy a configured .emscripten if needed
if [ ! -f "$HOME/.emscripten" -o ! -f "$HOME/.emscripten_sanity" ]; then
    echo "# Copying default .emscripten"
    cp -f ${HAM_TOOLSET_DIR}/etc/.emscripten $HOME/.emscripten
    emcc --version
fi

update_prompt
