#!/bin/bash -e
export HAM_TOOLSET=openxr_cppm
export HAM_TOOLSET_NAME=openxr_cppm
export HAM_TOOLSET_DIR="${HAM_HOME}/specs/toolsets/openxr_cppm"

case $HAM_OS in
  NT*) ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

BUILD=ra ham-cppm-build-check openxr_cppm noheader openxr_cppm openxr_cppm
BUILD=da ham-cppm-build-check openxr_cppm noheader openxr_cppm openxr_cppm

VER="--- openxr_cppm ---------------------"
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
