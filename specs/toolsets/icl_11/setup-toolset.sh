#!/bin/bash

# dependencies
toolset_import msvc_10_x86
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET_IS_SETUP_ICL_11=1
export HAM_TOOLSET=INTELC
export HAM_TOOLSET_VER=11
export HAM_TOOLSET_NAME=icl_11
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/icl_11"

# path setup
case $HAM_OS in
    NT*)
        export ICLDIR="${HAM_TOOLSET_DIR}/nt-x86"
        export ICL_ARCH=ia32
        export INCLUDE="`nativedir \"${ICLDIR}/include/$ICL_ARCH\"`;`nativedir \"${ICLDIR}/include\"`;$INCLUDE"
        export LIB="`nativedir \"${ICLDIR}/lib/$ICL_ARCH\"`;$LIB"
        export PATH="${ICLDIR}/bin/$ICL_ARCH":${PATH}
        if [ ! -e "$ICLDIR" ] || [ -z "`type -P icl`" ]; then
            echo "E/nt-x86 folder doesn't exist in the toolset"
            return 1
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- icl_11 ------------------------
`icl 2>&1 | grep 'Version' ; true`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

HAM_C99=icl
