#!/bin/bash

# toolset
export HAM_TOOLSET=WINDBG
export HAM_TOOLSET_NAME=windbg
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/windbg"

# path setup
case $HAM_OS in
    NT*)
        export WINDBG_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH=${WINDBG_DIR}/debugger_x86:${PATH}
        if [ ! -e "$WINDBG_DIR" ]; then
            toolset_dl windbg windbg_nt-x86
            if [ ! -e "$WINDBG_DIR" ]; then
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

VER="--- windbg ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
WinDebug"
    if [ $? != 0 ]; then
      echo "E/Can't get version."
      return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
