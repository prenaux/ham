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
        export PHP_HOME="/usr/local/opt/php@7.3/";
        if [ ! -e "$PHP_HOME/bin/php" ]; then
            echo "I/Brew PHP 7.3 not found, trying to install."
            brew install php@7.3
        fi
        export PATH="${HAM_TOOLSET_DIR}":"${HOME}/.composer/vendor/bin":"${PHP_HOME}/bin":${PATH}
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- php ------------------------
`php --version`
`composer -V`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
