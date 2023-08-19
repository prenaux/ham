#!/bin/bash

toolset_import xslt_tools
if [ $? != 0 ]; then return 1; fi

case $HAM_OS in
  NT*)
    export CLANG_DIR="/c/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Tools/Llvm/bin"
    ;;
  OSX*)
    export CLANG_DIR="/usr/local/Cellar/llvm/11.0.0/bin"
  # . ham-toolset-import.sh macos_x64
  ;;
  LINUX)
  # . ham-toolset-import.sh linux_x64
  ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

export HAM_TOOLSET=WINDOWS
export HAM_TOOLSET_VER=15
export HAM_TOOLSET_NAME=windows
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/windows"
export MSVC_DIR="${HAM_HOME}/toolsets/msvc_19_x64/nt-x86"

export PATH=${PATH}:${CLANG_DIR}

# export CLANG_EXE_BASE="C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Tools/Llvm/bin"

VER="--- clang -----------------------
`clang --version`"
if [ $? != 0 ]; then
  echo "E/Can't get version."
  return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
