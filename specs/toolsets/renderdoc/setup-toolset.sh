#!/bin/bash
export HAM_TOOLSET=RENDERDOC
export HAM_TOOLSET_NAME=renderdoc
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/renderdoc"

case $HAM_OS in
  LINUX)
    toolset_check_and_dl_ver renderdoc lin-x64 v1_35 || return 1
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

pathenv_add "${HAM_TOOLSET_DIR}/$HAM_BIN_LOA/bin"

# version
VER="--- renderdoc ----------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(renderdoccmd --version | grep renderdoccmd)"; then
    echo "E/Can't get renderdoc version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
