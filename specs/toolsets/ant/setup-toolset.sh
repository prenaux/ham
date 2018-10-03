#!/bin/bash
. ham-toolset-import.sh java_jdk18
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=ANT
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=ant
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/ant"

# path setup
export ANT_BIN_DIR="${HAM_TOOLSET_DIR}/jvm/bin"
export PATH="${ANT_BIN_DIR}":${PATH}
if [ ! -e "$ANT_BIN_DIR" ]; then
    toolset_dl ant ant_1_jvm
    if [ ! -e "$ANT_BIN_DIR" ]; then
        echo "E/jvm folder doesn't exist in the toolset"
        return 1
    fi
    chmod +x "$ANT_BIN_DIR/"*
fi

# path
export PATH="${HAM_TOOLSET_DIR}":${PATH}

# version
VER="--- ant ----------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`ant -version`"
    if [ $? != 0 ]; then
        echo "E/Can't get version."
        return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
