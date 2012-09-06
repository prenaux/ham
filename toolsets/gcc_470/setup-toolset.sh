#!/bin/bash

# toolset
export HAM_TOOLSET=GCC
export HAM_TOOLSET_VER=470
export HAM_TOOLSET_NAME=gcc_470
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/gcc_470

# path setup
case $HAM_OS in
    NT*)
        export GCCDIR=${HAM_TOOLSET_DIR}/nt-x86
        export INCLUDE=${GCCDIR}/include
        export LIB=${GCCDIR}/lib
        export PATH=${GCCDIR}/bin:${PATH}
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
