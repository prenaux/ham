#!/bin/bash

# import
case $HAM_OS in
    NT*)
        ;;
    OSX*)
        toolset_import_once wine || return 1
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# toolset
export HAM_TOOLSET=NICGC
export HAM_TOOLSET_NAME=nicgc
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/nicgc"

# path setup
case $HAM_OS in
    NT*)
        ;;
    OSX*)
        toolset_check_and_dl_ver nicgc osx-x64 v1 || return 1
        export NICGC_DIR="${HAM_TOOLSET_DIR}/osx-x64"
        pathenv_add "${NICGC_DIR}/bin"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# path
pathenv_add "${HAM_TOOLSET_DIR}"

# version
VER="--- nicgc ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`nicgc -v 2>&1`"
    if [ $? != 0 ]; then
      echo "E/Can't get nicgc version."
      return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
