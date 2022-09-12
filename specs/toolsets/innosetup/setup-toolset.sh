#!/bin/bash

# toolset
export HAM_TOOLSET=INNOSETUP
export HAM_TOOLSET_NAME=innosetup
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/innosetup"

# path setup
case $HAM_OS in
    NT*)
        export INNOSETUP_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH="${INNOSETUP_DIR}":${PATH}
        if [ ! -e "$INNOSETUP_DIR" ]; then
            toolset_dl innosetup innosetup_nt-x86
            if [ ! -e "$INNOSETUP_DIR" ]; then
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

VER="--- innosetup ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`iscc 2>&1 | grep 'Inno Setup'`"
    if [ $? != 0 ]; then
      echo "E/Can't get version."
      return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
