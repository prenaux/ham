#!/bin/bash

# toolset
export HAM_TOOLSET=INNOSETUP
export HAM_TOOLSET_NAME=innosetup
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/innosetup"

# path setup
case $HAM_OS in
  NT*)
    export INNOSETUP_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
    toolset_check_and_dl_ver innosetup nt-x86 v6_4_2 || return 1
    pathenv_add "${INNOSETUP_DIR}"
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

VER="--- innosetup ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(iscc 2>&1 | grep 'Inno Setup')"; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
