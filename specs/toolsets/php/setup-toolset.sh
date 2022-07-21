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
        export PHP_HOME=`ham-brew-installdir php@7.4`
        ham-brew-install php@7.4 "bin/php" || return 1
        export PATH="${HAM_TOOLSET_DIR}":"${HOME}/.composer/vendor/bin":"${PHP_HOME}/bin":${PATH}
        ;;
    LINUX*)
        if [ -z `where_inpath php` ]; then
            echo "W/php 7.4 not found, trying to install with sudo..."
            (set -x ; sudo apt-get install -y php7.4-common php7.4-mysql php7.4-xml php7.4-xmlrpc php7.4-curl php7.4-gd php7.4-imagick php7.4-cli php7.4-dev php7.4-imap php7.4-mbstring php7.4-opcache php7.4-pgsql php7.4-soap php7.4-sqlite3 php7.4-zip php7.4-intl)
        fi
        pathenv_add "${HAM_TOOLSET_DIR}"
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
