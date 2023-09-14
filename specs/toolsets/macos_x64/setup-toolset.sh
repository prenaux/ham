#!/bin/bash
toolset_import_once xslt_tools || return 1

case $HAM_OS in
  OSX*)
    # Added system sdk path using the xcode command line tool..
    export MACOS_SDK_PATH=$(xcrun --show-sdk-path)
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

# toolset
export HAM_TOOLSET=CLANG
export HAM_TOOLSET_NAME=clang_33
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}"
export HAM_CPP_TOOLSET=$HAM_TOOLSET
export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

export OSPLAT=X64
export BUILD_BIN_LOA=$HAM_BIN_LOA

# finding correct clang compiler dir
local dir=$(clang --version | grep InstalledDir)
export CMD_JSON_COMPILER_PATH=${dir#*' '}/

VER="--- macos_x64 -----------------------
$(clang -arch x86_64 --version)"
if [ $? != 0 ]; then
  echo "E/Can't get version."
  return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
