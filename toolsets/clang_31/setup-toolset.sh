#!/bin/bash

# toolset
export HAM_TOOLSET=CLANG
export HAM_TOOLSET_VER=31
export HAM_TOOLSET_NAME=clang_31
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}

# path setup
case $HAM_OS in
    NT*)
        export CLANGDIR=${HAM_TOOLSET_DIR}/nt-x86
        export INCLUDE=${CLANGDIR}/include
        export LIB=${CLANGDIR}/lib
        export PATH=${CLANGDIR}/bin:${PATH}
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
