#!/bin/bash

# toolset
export HAM_TOOLSET=PHP
export HAM_TOOLSET_VER=56
export HAM_TOOLSET_NAME=php
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/php"

# path setup
case $HAM_OS in
    NT*)
        export PHP_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH="${PHP_DIR}":"${HAM_TOOLSET_DIR}":${PATH}
        if [ ! -e "$PHP_DIR/php.exe" ]; then
            toolset_dl php php_nt-x86
            if [ ! -e "$PHP_DIR/php.exe" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    OSX*)
        # PHP comes with the OS... yup
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- php ------------------------
`php --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
