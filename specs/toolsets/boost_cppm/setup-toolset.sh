#!/bin/bash -e
export HAM_TOOLSET=boost_cppm
export HAM_TOOLSET_NAME=boost_cppm
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/boost_cppm"

export BOOST_CPPM_VER=1_84_0
export HAM_HDRS_BOOST_CPPM="${HAM_TOOLSET_DIR}/dist"
toolset_check_and_dl_ver boost_cppm dist v${BOOST_CPPM_VER} || return 1

VER="--- boost_cppm ----------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  ham-check-file "$HAM_HDRS_BOOST_CPPM/boost/config.hpp"
  errcheck $? $HAM_TOOLSET_NAME "Include check failed." || return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
