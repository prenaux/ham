#!/bin/bash
toolset_import_once python_3 || return 1
toolset_import_once nodejs || return 1
toolset_import_once xslt_tools || return 1

# toolset
export HAM_TOOLSET=EMSCRIPTEN
export HAM_TOOLSET_NAME=emscripten
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/emscripten"
export HAM_CPP_TOOLSET=$HAM_TOOLSET
export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

# explicitly set HAM_TARGET_BIN_LOA
export HAM_TARGET_BIN_LOA=web-js

# path setup
case $HAM_OS in
  OSX*)
    if [ -z "$(which emcc)" ]; then
      echo "W/Couldn't find emcc, will try to install it with brew."
      xcode-select --install
      ham-brew install emscripten
    fi
    pathenv_add "${HAM_TOOLSET_DIR}"
    export BUILD_BIN_LOA=$HAM_BIN_LOA
    ;;
  LINUX*)
    HAM_ENABLE_EXPERIMENTAL_LINUX_BREW=1 ham-brew-install emscripten "bin/emcc"
    pathenv_add "${HAM_TOOLSET_DIR}"
    export BUILD_BIN_LOA=$HAM_BIN_LOA
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

VER="--- emscripten ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(emcc --version | grep emcc)"; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
