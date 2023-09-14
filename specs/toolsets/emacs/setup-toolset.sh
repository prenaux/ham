#!/bin/bash

# toolset
export HAM_TOOLSET=EMACS
export HAM_TOOLSET_NAME=emacs
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/emacs"

export EMACS_EXE=emacs

# path setup
case $HAM_OS in
  NT*)
    export EMACS_EXE=emacs.exe
    export EMACS_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
    if [ ! -e "$EMACS_DIR" ]; then
      toolset_check_and_dl_ver emacs nt-x86 v28_2 || return 1
      if [ ! -e "$EMACS_DIR" ]; then
        echo "E/nt-x86 folder doesn't exist in the toolset"
        return 1
      fi
    fi
    pathenv_add "${EMACS_DIR}/bin"
    ;;
  OSX*)
    if [ -z $(which emacs)]; then
      echo "W/Couldn't find emacs, trying to install with brew"
      ham-brew install emacs
    fi
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

VER="--- emacs ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  VER="$VER
$(emacs --version | grep 'Emacs' | head -n 1)"
  if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
