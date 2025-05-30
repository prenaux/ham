#!/bin/bash
case $HAM_OS in
  LINUX) ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

export OSPLAT=X64
export BUILD_BIN_LOA=$HAM_BIN_LOA

if [ -n "$HAM_LINUX_CLANG" ]; then
  LINUX_CLANG=$HAM_LINUX_CLANG
elif [ "$(ham-clang-version)" == "18" ]; then
  # Default to system Clang 18 if detected.
  LINUX_CLANG=clang_system
else
  # Default to Clang 18.
  LINUX_CLANG=clang_18
fi

# Use GCC
if [ "${LINUX_CLANG}" == "gcc_system" ]; then
  ### system gcc ###
  toolset_import gcc_470 || return 1

  VER="--- linux_x64 -----------------------
Using system GCC: $(gcc --version | grep gcc)"

elif [ "${LINUX_CLANG}" == "clang_18" ]; then
  ### clang 18 ###
  toolset_import clang_18 || return 1

  VER="--- linux_x64 -----------------------
Using redist Clang 18: $(clang -arch x86_64 --version | grep version)"

elif [ "${LINUX_CLANG}" == "clang_19" ]; then
  ### clang 19 ###
  toolset_import clang_19 || return 1

  VER="--- linux_x64 -----------------------
Using redist Clang 19: $(clang -arch x86_64 --version | grep version)"

elif [ "${LINUX_CLANG}" == "clang_system" ]; then
  ### system clang ###
  DETECTED_CLANG_VERSION=$(ham-clang-version)
  case "$DETECTED_CLANG_VERSION" in
    15 | 16 | 17 | 18 | 19) ;;
    *)
      log_error "Unsupported system clang version '$DETECTED_CLANG_VERSION'."
      ;;
  esac

  export HAM_TOOLSET=CLANG
  export HAM_TOOLSET_NAME=clang_${DETECTED_CLANG_VERSION}
  export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}"
  export HAM_CPP_TOOLSET=$HAM_TOOLSET
  export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

  # finding correct clang compiler dir
  dir=$(clang --version | grep InstalledDir)
  export CMD_JSON_COMPILER_PATH=${dir#*' '}/

  if ! VER="--- linux_x64 -----------------------
Using system Clang: $(clang -arch x86_64 --version)"; then
    echo "E/Can't get version."
    return 1
  fi

else
  ### Unkown Linux C/C++ compiler ###
  log_error "Unknown LINUX_CLANG: '$LINUX_CLANG'."
  return 1
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
