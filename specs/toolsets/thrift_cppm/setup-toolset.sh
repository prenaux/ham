#!/bin/bash -e
export HAM_TOOLSET=thrift_cppm
export HAM_TOOLSET_NAME=thrift_cppm
export HAM_TOOLSET_DIR="${HAM_HOME}/specs/toolsets/thrift_cppm"

export HAM_HDRS_THRIFT_CPPM="${HAM_TOOLSET_DIR}/github-thrift/lib/cpp/src"

BUILD=ra ham-cppm-build-check thrift_cppm noheader thrift_cppm thrift_cppm || return 1
BUILD=da ham-cppm-build-check thrift_cppm noheader thrift_cppm thrift_cppm || return 1

HAM_THRIFTC_EXE_PATH=$(BUILD_TARGET=default ham-cppm-exe-path thrift_cppm ham_thriftc)

VER="--- thrift_cppm ---------------------
$(${HAM_THRIFTC_EXE_PATH} --version)"
errcheck $? toolset_thrift_cppm "Can't get ham_thriftc version." || return 1
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
