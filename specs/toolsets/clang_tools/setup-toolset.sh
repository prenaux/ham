#!/bin/bash

# toolset
export HAM_TOOLSET=clang_tools
export HAM_TOOLSET_NAME=clang_tools
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/clang_tools"

case "$HAM_BIN_LOA" in
  osx-x64 | osx-arm64)
    ham-brew-install llvm "bin/clang-query"
    pathenv_add "$(ham-brew-installdir llvm/bin)"
    ;;
  *)
    complain clang_tools "Unsupported arch '$HAM_BIN_LOA'."
    return 1
    ;;
esac
pathenv_add "$HAM_TOOLSET_DIR"

VER="--- clang_tools ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(clang-tidy --version | grep version)"; then
    echo "E/Can't get version."
    return 1
  fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
