#!/bin/bash

# toolset
export HAM_TOOLSET=MYSQL
export HAM_TOOLSET_NAME=mysql
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/mysql"

# path setup
case $HAM_OS in
    NT*)
        export MYSQL_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        if [ ! -e "$MYSQL_DIR" ]; then
            toolset_dl mysql mysql_nt-x86
            if [ ! -e "$MYSQL_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        pathenv_add "${MYSQL_DIR}/bin"
        ;;
    OSX*)
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# path
pathenv_add "${HAM_TOOLSET_DIR}"

export MYSQL_DB_DIR="$WORK/Server/mysql"

mkdir -p "$MYSQL_DB_DIR"
mkdir -p "$MYSQL_DB_DIR/data"

VER="--- mysql ---------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`mysql --version`"
    if [ $? != 0 ]; then
      echo "E/Can't get version."
      return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
