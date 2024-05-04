#!/bin/bash

# toolset
export HAM_TOOLSET=cosmocc
export HAM_TOOLSET_NAME=cosmocc
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/cosmocc"

# path setup
export COSMOCC_DIR="${HAM_TOOLSET_DIR}/dist"
toolset_check_and_dl_ver cosmocc dist v3_3_3 || return 1

# path
pathenv_add "${COSMOCC_DIR}/bin"

# cpp compiler
export HAM_CPP_TOOLSET=COSMOCC
export HAM_CPP_TOOLSET_NAME=cosmocc

# version
VER="--- cosmocc -------------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(cosmocc --version | grep 'cosmocc ')"; then
    echo "E/Can't cosmocc zig version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
