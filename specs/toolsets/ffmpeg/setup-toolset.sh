#!/bin/bash

# toolset
export HAM_TOOLSET=FFMPEG
export HAM_TOOLSET_NAME=ffmpeg
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/ffmpeg"

# path setup
case $HAM_OS in
  NT*)
    toolset_check_and_dl_ver ffmpeg nt-x64 v7_1 || return 1
    export FFMPEG_DIR="${HAM_TOOLSET_DIR}/nt-x64/"
    pathenv_add "${FFMPEG_DIR}/bin"
    ;;
  OSX*)
    toolset_check_and_dl_ver ffmpeg "$HAM_BIN_LOA" v4 || return 1
    export FFMPEG_DIR="${HAM_TOOLSET_DIR}/$HAM_BIN_LOA/"
    pathenv_add "${FFMPEG_DIR}"
    ;;
  LINUX)
    # ffmpeg is setup by ham-install-os-packages
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

# path
pathenv_add "${HAM_TOOLSET_DIR}"

VER="--- ffmpeg ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(ffmpeg 2>&1 | grep 'ffmpeg version')"; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
