#!/bin/bash

# toolset
export HAM_TOOLSET=BITKEEPER
export HAM_TOOLSET_NAME=bitkeeper
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/bitkeeper"

# platform
case $HAM_OS in
    NT*)
        toolset_check_and_dl_ver bitkeeper nt-x86 v1 || return 1
        export BITKEEPER_BIN_DIR="${HAM_HOME}/toolsets/bitkeeper/nt-x86"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# path
export PATH="${BITKEEPER_BIN_DIR}":${PATH}

# version
VER="--- bitkeeper ----------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`bk --version | grep 'BitKeeper version'`"
    if [ $? != 0 ]; then
        echo "E/Can't get version."
        return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
