#!/bin/bash -e
export HAM_TOOLSET=grpc_cppm
export HAM_TOOLSET_NAME=grpc_cppm
export HAM_TOOLSET_DIR="${HAM_HOME}/specs/toolsets/grpc_cppm"

export HAM_HDRS_GRPC_CPPM="${HAM_TOOLSET_DIR}/github-grpc"

CHECK_HEADER="${HAM_HDRS_GRPC_CPPM}/include/grpc/grpc.h"
CHECK_DLL="${HAM_HOME}/bin/$(ham-cppm-bin-filepath dll grpc_cppm_ra)"
if [ ! -e "$CHECK_DLL" ] || [ ! -e "$CHECK_HEADER" ]; then
  ham-cppm-build grpc_cppm
  if [ ! -e "$CHECK_DLL" ]; then
    echo "E/Can't find build artifact '$CHECK_DLL'."
    return 1
  fi
  if [ ! -e "$CHECK_HEADER" ]; then
    echo "E/Can't find build artifact '$CHECK_HEADER'."
    return 1
  fi
fi

VER="--- grpc_cppm ---------------------"
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
