#!/bin/bash

# toolset
export HAM_TOOLSET=TEXTEDITOR
export HAM_TOOLSET_NAME=texteditor
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/texteditor"

# path setup
case $HAM_OS in
  NT*)
    toolset_check_and_dl_ver texteditor nt-x86 v5 || return 1
    export TEXTEDITOR_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
    pathenv_add "${HAM_TOOLSET_DIR}"
    ;;
  *)
    ;;
esac

VER="--- texteditor ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
Text Editor"; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
