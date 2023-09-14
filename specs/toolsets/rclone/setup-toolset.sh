#!/bin/bash
export HAM_TOOLSET=RCLONE
export HAM_TOOLSET_NAME=rclone
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/rclone"

# dist
toolset_check_and_dl_ver rclone dist v1 || return 1
export RCLONE_DIST_DIR="${HAM_TOOLSET_DIR}/dist"

# path
pathenv_add "${RCLONE_DIST_DIR}/$HAM_BIN_LOA/"
pathenv_add "${HAM_TOOLSET_DIR}"

# version
VER="--- rclone ----------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(rclone --version | grep rclone)"; then
    echo "E/Can't get rclone version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
