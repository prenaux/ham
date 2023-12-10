#!/bin/bash

# toolset
export HAM_TOOLSET=PHP
export HAM_TOOLSET_NAME=php_8
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/php_8"

# path setup
case $HAM_OS in
  OSX*)
    export HAM_PHP_VERSION=8.2
    PHP_HOME=$(ham-brew-installdir php@$HAM_PHP_VERSION)
    export PHP_HOME
    # freetds, a dependency of php@$HAM_PHP_VERSION, needs brew's curl and for some
    # reason complains about it and fail the install instead of having it
    # as a dependency...
    ham-brew-install curl "bin/curl" || return 1
    ham-brew-install php@$HAM_PHP_VERSION "bin/php" || return 1
    pathenv_add "${PHP_HOME}/bin"
    pathenv_add "${HOME}/.composer/vendor/bin"
    export HAM_PHP_EXE_PATH="$PHP_HOME/bin/php"
    export HAM_PHP_FPM_EXE_PATH="$PHP_HOME/sbin/php-fpm"
    ;;
  LINUX*)
    export HAM_PHP_VERSION=8.2
    if [ -z "$(where_inpath "php$HAM_PHP_VERSION")" ]; then
      echo "W/php $HAM_PHP_VERSION not found, trying to install with sudo..."
      (
        set -ex
        sudo add-apt-repository -y ppa:ondrej/php
        sudo apt-get -y update
        ham-apt-get-install php$HAM_PHP_VERSION-common php$HAM_PHP_VERSION-mysql php$HAM_PHP_VERSION-xml php$HAM_PHP_VERSION-xmlrpc php$HAM_PHP_VERSION-curl php$HAM_PHP_VERSION-gd php$HAM_PHP_VERSION-imagick php$HAM_PHP_VERSION-cli php$HAM_PHP_VERSION-dev php$HAM_PHP_VERSION-imap php$HAM_PHP_VERSION-mbstring php$HAM_PHP_VERSION-opcache php$HAM_PHP_VERSION-pgsql php$HAM_PHP_VERSION-soap php$HAM_PHP_VERSION-sqlite3 php$HAM_PHP_VERSION-zip php$HAM_PHP_VERSION-intl php$HAM_PHP_VERSION-fpm php$HAM_PHP_VERSION-gmp
      ) || return 1
    fi
    export HAM_PHP_EXE_PATH="/usr/bin/php$HAM_PHP_VERSION"
    export HAM_PHP_FPM_EXE_PATH="/usr/sbin/php-fpm$HAM_PHP_VERSION"
    ;;
  NT*)
    export PHP_HOME="${HAM_TOOLSET_DIR}/nt-x64"
    export HAM_PHP_VERSION=8.2
    export HAM_PHP_EXE_PATH="$PHP_HOME/php.exe"
    export HAM_PHP_FPM_EXE_PATH="$PHP_HOME/php-fpm.exe"
    toolset_check_and_dl_ver php_8 nt-x64 v2 || return 1
    pathenv_add "${PHP_HOME}/bin"
    pathenv_add "${HOME}/.composer/vendor/bin"
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

pathenv_add "${HAM_HOME}/toolsets/php"
pathenv_add "${HAM_TOOLSET_DIR}"

VER="--- php_8 ----------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(php --version)
$(composer -V)"; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
