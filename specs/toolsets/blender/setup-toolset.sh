#!/bin/bash

# toolset
export HAM_TOOLSET=BLENDER
export HAM_TOOLSET_NAME=blender
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/blender"

export BLENDER_EXE=blender

# path setup
case $HAM_OS in
  NT*)
    export BLENDER_EXE=blender.exe
    export BLENDER_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
    toolset_check_and_dl_ver blender nt-x86 v4_3_2 || return 1
    pathenv_add "${BLENDER_DIR}"
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

VER="--- blender ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(blender --version | grep 'Blender')"; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
