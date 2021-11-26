#!/bin/bash
. ham-toolset-import.sh python_36 # for postgres_pass/postgres_connect/pgpass.py

# toolset
export HAM_TOOLSET=POSTGRES
export HAM_TOOLSET_VER=9
export HAM_TOOLSET_NAME=postgres
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/postgres"

# path setup
case $HAM_OS in
    NT*)
        export POSTGRES_DIR="${HAM_TOOLSET_DIR}/nt-x86"
        if [ ! -e "$POSTGRES_DIR" ]; then
            toolset_dl postgres postgres_nt-x86
            if [ ! -e "$POSTGRES_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        export PATH="${POSTGRES_DIR}/bin":${PATH}
        ;;
    OSX*)
        export POSTGRES_DIR="`brew --prefix postgresql@10`"
        if [ ! -e "$POSTGRES_DIR/bin/postgres" ]; then
            echo "I/Brew postgresql@10 not found, trying to install."
            ham-brew install postgresql@10
            if [ ! -e "$POSTGRES_DIR/bin/postgres" ]; then
                echo "E/Brew postgresql@10 install failed."
                return 1
            fi
        fi
        export PATH="${POSTGRES_DIR}/bin":${PATH}
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
