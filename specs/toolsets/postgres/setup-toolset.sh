#!/bin/bash

# toolset
export HAM_TOOLSET=POSTGRES
export HAM_TOOLSET_VER=9
export HAM_TOOLSET_NAME=postgres
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/postgres"

# path setup
case $HAM_OS in
    NT*)
        export POSTGRES_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH="${POSTGRES_DIR}/bin":${PATH}
        if [ ! -e "$POSTGRES_DIR" ]; then
            toolset_dl postgres postgres_nt-x86
            if [ ! -e "$POSTGRES_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    OSX*)
        export POSTGRES_DIR="${HAM_TOOLSET_DIR}/osx-x64/"
        export PATH="${POSTGRES_DIR}/bin":${PATH}
        if [ ! -e "$POSTGRES_DIR" ]; then
            toolset_dl postgres postgres_osx-x64
            if [ ! -e "$POSTGRES_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# path
export PATH="${HAM_TOOLSET_DIR}":${PATH}
export POSTGRES_DB_DIR="$WORK/Server/pg"

VER="--- postgres ---------------------
`postgres --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
