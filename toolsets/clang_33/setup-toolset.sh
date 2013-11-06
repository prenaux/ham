#!/bin/bash
toolset_import gcc_470
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=CLANG
export HAM_TOOLSET_VER=33
export HAM_TOOLSET_NAME=clang_33
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}

# path setup
case $HAM_OS in
    NT*)
        export CLANGDIR=${HAM_TOOLSET_DIR}/nt-x86
        export PATH=${CLANGDIR}/bin:${PATH}
        if [ ! -e $CLANGDIR ] || [ -z `type -P clang` ]; then
            toolset_dl clang_33 clang_33_nt-x86
            if [ ! -e $CLANGDIR ] || [ -z `type -P clang` ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    OSX)
        export RUN_DEBUGGER=lldb
        export RUN_DEBUGGER_PARAMS=-f
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- clang_33 ------------------------
`clang --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
