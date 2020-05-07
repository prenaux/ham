#!/bin/bash

# toolset
export HAM_TOOLSET=PHP
export HAM_TOOLSET_VER=73
export HAM_TOOLSET_NAME=php
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/php"

# path setup
case $HAM_OS in
    NT*)
        export PHP_HOME="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH="${PHP_HOME}":"${HAM_TOOLSET_DIR}":${PATH}
        toolset_check_and_dl_ver php nt-x86 v7_3 || return 1
        ;;
    OSX*)
        export PATH="${HAM_TOOLSET_DIR}":${PATH}
        # PHP comes with the OS... yup
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- php ------------------------
`php --version`
`php_composer -V`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
