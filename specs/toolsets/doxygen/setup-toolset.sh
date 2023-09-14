#!/bin/bash

# toolset
export HAM_TOOLSET=DOXYGEN
export HAM_TOOLSET_NAME=doxygen
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/doxygen"

# path setup
case $HAM_OS in
  NT*)
    export DOXYGEN_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
    if [ ! -e "$DOXYGEN_DIR/doxygen_187.exe" ]; then
      toolset_dl doxygen doxygen_nt-x86
      if [ ! -e "$DOXYGEN_DIR" ]; then
        echo "E/nt-x86 folder doesn't exist in the toolset"
        return 1
      fi
    fi
    pathenv_add "${DOXYGEN_DIR}"
    ;;
  OSX*)
    export DOXYGEN_DIR="${HAM_TOOLSET_DIR}/osx-x86/Resources"
    if [ ! -e "$DOXYGEN_DIR/doxygen" ]; then
      toolset_dl doxygen doxygen_osx-x86
      if [ ! -e "$DOXYGEN_DIR" ]; then
        echo "E/osx-x86 folder doesn't exist in the toolset"
        return 1
      fi
    fi
    pathenv_add "${DOXYGEN_DIR}"
    VER="$(doxygen_187 --version)"
    if [ $? != 0 ]; then
      rm -f "$DOXYGEN_DIR/doxygen_187"
    fi
    if [ ! -e "$DOXYGEN_DIR/doxygen_187" ]; then
      ln -s "$DOXYGEN_DIR/doxygen" "$DOXYGEN_DIR/doxygen_187"
    fi
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

VER="--- doxygen -------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  VER="$VER
$(doxygen_187 --version)"
  if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
