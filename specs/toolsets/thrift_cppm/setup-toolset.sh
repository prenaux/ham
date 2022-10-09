#!/bin/bash -e
export HAM_TOOLSET=thrift_cppm
export HAM_TOOLSET_NAME=thrift_cppm
export HAM_TOOLSET_DIR="${HAM_HOME}/specs/toolsets/thrift_cppm"

export HAM_HDRS_THRIFT_CPPM="${HAM_TOOLSET_DIR}/github-thrift/lib/cpp/src"

CHECK_HEADER="${HAM_HDRS_THRIFT_CPPM}/thrift/TBase.h"
CHECK_DLL="${HAM_HOME}/bin/`ham-cppm-bin-filepath dll thrift_cppm_ra`"
CHECK_LIB="${HAM_HOME}/libs/`ham-cppm-bin-filepath lib thrift_cppm_ra`"
if [ ! -e "$CHECK_DLL" ] || [ ! -e "$CHECK_LIB" ] || [ ! -e "$CHECK_HEADER" ]; then
  ham-cppm-build thrift_cppm
  if [ ! -e "$CHECK_DLL" ]; then
    echo "E/Can't find build artifact '$CHECK_DLL'."
    return 1
  fi
  if [ ! -e "$CHECK_LIB" ]; then
    echo "E/Can't find build artifact '$CHECK_LIB'."
    return 1
  fi
  if [ ! -e "$CHECK_HEADER" ]; then
    echo "E/Can't find build artifact '$CHECK_HEADER'."
    return 1
  fi
fi

VER="--- thrift_cppm ---------------------"
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
