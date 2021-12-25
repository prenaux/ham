#!/bin/bash

toolset_import python_27
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=SCONS
export HAM_TOOLSET_NAME=scons
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/scons"

# python
toolset_check_and_dl_ver scons python v1 || return 1

export PATH="${HAM_HOME}/toolsets/scons/python":$PATH

# version
VER="--- scons --------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`scons --version | grep 'script: ' | sed 's|^[[:blank:]]*||g'`"
    if [ $? != 0 ]; then
        echo "E/Can't get scons version."
        return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
