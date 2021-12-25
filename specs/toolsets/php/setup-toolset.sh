#!/bin/bash

# toolset
export HAM_TOOLSET=PHP
export HAM_TOOLSET_NAME=php
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/php"

# path setup
case $HAM_OS in
    NT*)
        export PHP_HOME="${HAM_TOOLSET_DIR}/nt-x86"
        export PATH="${PHP_HOME}":"${HAM_TOOLSET_DIR}":${PATH}
        toolset_check_and_dl_ver php nt-x86 v7_3 || return 1
        if [ ! -e "C:/Windows/php.ini" ]; then
            echo "!!!"
            echo "!!! You have to copy $PHP_HOME/php_ini_for_c_windows/php-ini-nt-x86.ini to"
            echo "!!! C:/Windows/php.ini with the Windows Explorer as it requires admin rights."
            echo "!!!"
            pushd "$PHP_HOME/../php_ini_for_c_windows"
            explorer .
            popd
            pushd "C:/Windows"
            explorer .
            popd
        fi
        ;;
    OSX*)
        export PHP_HOME="`brew --prefix php@7.4`";
        if [ ! -e "$PHP_HOME/bin/php" ]; then
            echo "I/Brew php@7.4 not found, trying to install."
            ham-brew install php@7.4
            if [ ! -e "$PHP_HOME/bin/php" ]; then
                echo "E/Brew php@7.4 install failed."
                return 1
            fi
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
