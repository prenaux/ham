#!/bin/bash

# toolset
export HAM_TOOLSET=POSTGRES
export HAM_TOOLSET_VER=9
export HAM_TOOLSET_NAME=postgres
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/postgres"

# path setup
case $HAM_OS in
    OSX*)
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# path
export PATH="${HAM_TOOLSET_DIR}":${PATH}

VER="--- postgres ---------------------
`postgres --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
