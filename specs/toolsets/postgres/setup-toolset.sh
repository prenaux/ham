#!/bin/bash

# for postgres_pass/postgres_connect/pgpass.py
toolset_import_once python_3 || return 1

# toolset
export HAM_TOOLSET=POSTGRES
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
        pathenv_add "${POSTGRES_DIR}/bin"
        ;;
    OSX*)
        ham-brew-install postgresql@10 "bin/postgres"
        export POSTGRES_DIR="`ham-brew-installdir postgresql@10`"
        pathenv_add "${POSTGRES_DIR}/bin"
        ;;
    LINUX*)
        if [ -z "`which psql`" ]; then
            sudo apt install -y postgresql-client
            if [ -z "`which psql`" ]; then
                echo "E/Brew postgresql@10 install failed."
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
pathenv_add "${HAM_TOOLSET_DIR}"
export POSTGRES_DB_DIR="$WORK/Server/pg"

VER="--- psql -------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`psql --version`"
    if [ $? != 0 ]; then
      echo "E/Can't get version."
      return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
