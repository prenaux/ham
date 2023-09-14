#!/bin/bash

# toolset
export HAM_TOOLSET=CG
export HAM_TOOLSET_NAME=cg
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/cg"

# path setup
case $HAM_OS in
  NT*)
    export CG_BIN_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
    if [ ! -e "$CG_BIN_DIR/cgc.exe" ]; then
      toolset_dl cg cg_nt-x86
      if [ ! -e "$CG_BIN_DIR/cgc.exe" ]; then
        echo "E/nt-x86 folder doesn't exist in the toolset"
        return 1
      fi
    fi
    pathenv_add "${CG_BIN_DIR}"
    ;;
  OSX*)
    export CG_BIN_DIR="${HAM_TOOLSET_DIR}/osx-x86/"
    if [ ! -e "$CG_BIN_DIR/cgc" ]; then
      toolset_dl cg cg_osx-x86
      if [ ! -e "$CG_BIN_DIR/cgc" ]; then
        echo "E/osx-x86 folder doesn't exist in the toolset"
        return 1
      fi
    fi
    pathenv_add "${CG_BIN_DIR}"
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

# docs
export CG_DOCS_DIR="${HAM_TOOLSET_DIR}/docs/"
if [ ! -e "$CG_DOCS_DIR/license.pdf" ]; then
  toolset_dl cg cg_docs
  if [ ! -e "$CG_DOCS_DIR/license.pdf" ]; then
    echo "E/docs folder doesn't exist in the toolset"
    return 1
  fi
fi

# path
pathenv_add "${HAM_TOOLSET_DIR}"

# version
VER="--- cg ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(cgc -v 2>&1)"; then
    echo "E/Can't get cg version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
