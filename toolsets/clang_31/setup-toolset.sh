#!/bin/bash
toolset_import gcc_470
if [ $? != 0 ]; then return 1; fi

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
        if [ ! -e $CLANGDIR ] || [ -z `type -P clang` ]; then
            toolset_dl clang_31 clang_31_nt-x86
            if [ ! -e $CLANGDIR ] || [ -z `type -P clang` ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- clang_31 ------------------------
`clang --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
