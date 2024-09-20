#!/bin/bash
toolset_import_once xslt_tools || return 1

case $HAM_OS in
  LINUX) ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

export OSPLAT=X64
export BUILD_BIN_LOA=$HAM_BIN_LOA

# Default to Clang 18 because we install it ourselve so its a reliable well
# defined version.
export LINUX_CLANG=${LINUX_CLANG:-18}

# Use GCC
if [ "${LINUX_CLANG}" == "0" ]; then
  toolset_import gcc_470 || return 1

  VER="--- linux_x64 -----------------------
Using GCC System: $(gcc --version | grep gcc)"

elif [ "${LINUX_CLANG}" == "18" ]; then
  toolset_import clang_18 || return 1

  VER="--- linux_x64 -----------------------
Using Clang 18: $(clang -arch x86_64 --version)"

else
  # Use the system clang
  export HAM_TOOLSET=CLANG
  export HAM_TOOLSET_NAME=clang_33
  export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}"
  export HAM_CPP_TOOLSET=$HAM_TOOLSET
  export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

  # finding correct clang compiler dir
  dir=$(clang --version | grep InstalledDir)
  export CMD_JSON_COMPILER_PATH=${dir#*' '}/

  if ! VER="--- linux_x64 -----------------------
Using Clang System: $(clang -arch x86_64 --version)"; then
    echo "E/Can't get version."
    return 1
  fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
