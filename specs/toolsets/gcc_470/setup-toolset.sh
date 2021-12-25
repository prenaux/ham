#!/bin/bash

# toolset
export HAM_TOOLSET=GCC
export HAM_TOOLSET_NAME=gcc_470
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/gcc_470"

# path setup
case $HAM_OS in
    NT*)
        export GCCDIR="${HAM_TOOLSET_DIR}/nt-x86"
        export PATH="${GCCDIR}/bin":${PATH}
        if [ ! -e "$GCCDIR" ] || [ -z "`type -P gcc`" ]; then
            toolset_dl gcc_470 gcc_470_nt-x86
            if [ ! -e "$GCCDIR" ] || [ -z "`type -P gcc`" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    OSX)
        ;;
    LINUX)
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;

esac

VER="--- gcc_470 ------------------------
`gcc --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
