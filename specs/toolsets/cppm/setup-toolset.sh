#!/bin/bash -e
BUILD_TARGET=${BUILD_TARGET:-default}

CPP_TOOLSET=$(ham-cppm-get-toolset "${BUILD_TARGET}")
errcheck $? toolset_cppm "cppm: Can't get C++ toolset for BUILD_TARGET '$BUILD_TARGET'." || return 1
echo "I/cppm: BUILD_TARGET=${BUILD_TARGET}, CPP_TOOLSET=${CPP_TOOLSET}"

toolset_import_once "${CPP_TOOLSET}" || return 1
