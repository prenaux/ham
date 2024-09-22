#!/bin/bash
toolset_import_once xslt_tools || return 1

export HAM_TOOLSET=CLANG
export HAM_TOOLSET_NAME=clang_19
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}"
export HAM_CPP_TOOLSET=$HAM_TOOLSET
export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

case $HAM_OS in
  LINUX)
    toolset_check_and_dl_ver clang_19 lin-x64 v19_1_0 || return 1
    export CLANG_DIR="${HAM_TOOLSET_DIR}/lin-x64"
    export OSPLAT=X64
    export BUILD_BIN_LOA=$HAM_BIN_LOA
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

pathenv_add "${CLANG_DIR}/bin"

# finding correct clang compiler dir
dir=$(clang --version | grep InstalledDir)
export CMD_JSON_COMPILER_PATH=${dir#*' '}/

VER="--- clang_19 ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(clang -arch x86_64 --version)"; then
    echo "E/Can't get version."
    return 1
  fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
