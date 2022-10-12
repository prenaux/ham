#!/bin/bash -e
export HAM_TOOLSET=thrift_cppm
export HAM_TOOLSET_NAME=thrift_cppm
export HAM_TOOLSET_DIR="${HAM_HOME}/specs/toolsets/thrift_cppm"

export HAM_HDRS_THRIFT_CPPM="${HAM_TOOLSET_DIR}/github-thrift/lib/cpp/src"

ham-cppm-build-check thrift_cppm noheader || return 1

VER="--- thrift_cppm ---------------------"
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
