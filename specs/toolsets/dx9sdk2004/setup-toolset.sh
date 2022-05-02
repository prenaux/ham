#!/bin/bash
export HAM_TOOLSET=DX9SDK2004
export HAM_TOOLSET_NAME=dx9sdk2004
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/dx9sdk2004"

# dist
toolset_check_and_dl_ver dx9sdk2004 dist v1 || return 1

# version
VER="--- dx9sdk2004 ----------------------------"
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
