#!/bin/bash
export HAM_TOOLSET=RCLONE
export HAM_TOOLSET_NAME=rclone
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/rclone"

# dist
toolset_check_and_dl_ver rclone dist v1 || return 1
export RCLONE_DIST_DIR="${HAM_TOOLSET_DIR}/dist"

# path
export PATH="${HAM_TOOLSET_DIR}":"${RCLONE_DIST_DIR}/$HAM_BIN_LOA/":${PATH}

# version
VER="--- rclone ----------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`rclone --version | grep rclone`"
    if [ $? != 0 ]; then
        echo "E/Can't get rclone version."
        return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
