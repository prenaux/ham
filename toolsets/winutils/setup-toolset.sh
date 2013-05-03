#!/bin/bash

# toolset
export HAM_TOOLSET=WINUTILS
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=winutils
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/winutils

# path setup
case $HAM_OS in
    NT*)
        export WINUTILS_DIR=${HAM_TOOLSET_DIR}/nt-x86/
        export PATH=${HAM_TOOLSET_DIR}/:${PATH}
        if [ ! -e "$WINUTILS_DIR" ]; then
            toolset_dl winutils winutils_nt-x86
            if [ ! -e "$WINUTILS_DIR" ]; then
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

VER="--- winutils ------------------------
Various Windows utilities: Notepad++, Network Monitor, Sysinternals,
Process Explorer, Depends, Sniffer, CPU-Z, HexEdit, SumatraPDF, ..."
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
