#!/bin/bash
export HAM_TOOLSET=slangc
export HAM_TOOLSET_NAME=slangc
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/slangc"

case "$HAM_BIN_LOA" in
  lin-x64 | osx-x64 | osx-arm64)
    toolset_check_and_dl_ver slangc "$HAM_BIN_LOA" v25_3_1 || return 1
    HAM_TOOLSET_DIR_SLANGC="$HAM_TOOLSET_DIR/$HAM_BIN_LOA"
    ;;
  nt-x86 | nt-x64)
    toolset_check_and_dl_ver slangc nt-x64 v25_3_1 || return 1
    HAM_TOOLSET_DIR_SLANGC="$HAM_TOOLSET_DIR/nt-x64"
    ;;
  *)
    complain slangc "Unsupported arch '$HAM_BIN_LOA'."
    return 1
    ;;
esac
pathenv_add "$HAM_TOOLSET_DIR_SLANGC/bin"

VER="--- slangc ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
slangc: $(slangc -v 2>&1)"; then
    echo "E/Can't get version."
    return 1
  fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
