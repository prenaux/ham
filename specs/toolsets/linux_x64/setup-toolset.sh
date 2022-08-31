#!/bin/bash

. ham-toolset-import.sh xslt_tools
if [ $? != 0 ]; then return 1; fi

case $HAM_OS in
    LINUX)
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

export LINUX_CLANG=${LINUX_CLANG:-1}
export OSPLAT=X64
export BUILD_BIN_LOA=$HAM_BIN_LOA

# Use GCC
if [ "${LINUX_CLANG}" == "0" ]; then
  toolset_import gcc_470
  if [ $? != 0 ]; then return 1; fi

  VER="--- linux_x64 -----------------------
Using GCC"
else
  # Use clang
  export HAM_TOOLSET=CLANG
  export HAM_TOOLSET_NAME=clang_33
  export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}"
  export HAM_CPP_TOOLSET=$HAM_TOOLSET
  export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

  # finding correct clang compiler dir
  local dir=$(clang --version | grep InstalledDir)
  export CMD_JSON_COMPILER_PATH=${dir#*' '}/

  VER="--- linux_x64 -----------------------
`clang -arch x86_64  --version`"
  if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
  fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
