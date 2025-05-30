#!/bin/bash
if [ -z "$HAM_TARGET_BIN_LOA" ]; then
  complain zig "HAM_TARGET_BIN_LOA not exported."
  return 1
fi

# toolset
export HAM_TOOLSET=zig
export HAM_TOOLSET_NAME=zig
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/zig"

# path setup
case $HAM_BIN_LOA in
  nt-x86 | lin-x64 | osx-arm64 | osx-x64)
    export ZIG_DIR="${HAM_TOOLSET_DIR}/$HAM_BIN_LOA"
    toolset_check_and_dl_ver zig "$HAM_BIN_LOA" v0_14_0 || return 1
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

# per-os path setup
case $HAM_BIN_LOA in
  osx-*)
    # Added system sdk path using the xcode command line tool..
    MACOS_SDK_PATH=$(xcrun --show-sdk-path)
    export MACOS_SDK_PATH
    ;;
esac

# path
pathenv_add "${HAM_TOOLSET_DIR}/$HAM_BIN_LOA"
pathenv_add "${HAM_TOOLSET_DIR}"

# cpp compiler
export HAM_CPP_TOOLSET=ZIGCC
export HAM_CPP_TOOLSET_NAME=zigcc
export BUILD_BIN_LOA=$HAM_BIN_LOA

# version
VER="--- zig ${HAM_TARGET_BIN_LOA} -------------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
zig: $(zig version)"; then
    echo "E/Can't get zig version."
    return 1
  fi
  if ! VER="$VER
zig cc: $(zig cc --version | grep version)"; then
    echo "E/Can't get zig cc version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
