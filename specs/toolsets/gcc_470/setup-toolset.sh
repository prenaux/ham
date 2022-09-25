#!/bin/bash

# toolset
export HAM_TOOLSET=GCC
export HAM_TOOLSET_NAME=gcc_470
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/gcc_470"
export HAM_CPP_TOOLSET=$HAM_TOOLSET
export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

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
    LINUX*)
        export GCC_VERSION=12
        ham-brew-install gcc@${GCC_VERSION} "bin/gcc-${GCC_VERSION}" || return 1
        export GCC_CC="gcc-${GCC_VERSION}"
        export GCC_CPP="g++-${GCC_VERSION}"
        export GCC_LINK="g++-${GCC_VERSION}"
        export GCC_AR="gcc-ar-${GCC_VERSION}"
        export GLIBC_INCDIR=`ham-brew-installdir glibc include`
        export GLIBC_LIBDIR=`ham-brew-installdir glibc lib`
        export LIBUNWIND_INCDIR=`ham-brew-installdir libunwind include`
        export LIBUNWIND_LIBDIR=`ham-brew-installdir libunwind lib`
        export GCC_DEBUG_FLAG=-gdwarf-2

        ham-brew-install libunwind "lib/libunwind.a" || return 1
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- gcc_470 ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  ham-check-file "$GLIBC_INCDIR/stdio.h"
  errcheck $? $HAM_TOOLSET_NAME "Include check failed." || return 1
  ham-check-file "$GLIBC_LIBDIR/libc.a"
  errcheck $? $HAM_TOOLSET_NAME "Lib check failed." || return 1

  VER="$VER
`$GCC_CC --version`"
  if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
