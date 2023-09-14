#!/bin/bash

# toolset
export HAM_TOOLSET=PHP
export HAM_TOOLSET_NAME=php
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/php"

echo "W/======================================================================="
echo "W/ The php toolset is deprecated and will be removed in the near future. "
echo "W/ Use php_8 instead."
echo "W/======================================================================="

export HAM_PHP_VERSION=7.4

# path setup
case $HAM_OS in
  NT*)
    export PHP_HOME="${HAM_TOOLSET_DIR}/nt-x86"
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
    pathenv_add "${PHP_HOME}"
    pathenv_add "${HAM_TOOLSET_DIR}"
    ;;
  OSX*)
    PHP_HOME=$(ham-brew-installdir php@7.4)
    export PHP_HOME
    # freetds, a dependency of php@7.4, needs brew's curl and for some
    # reason complains about it and fail the install instead of having it
    # as a dependency...
    ham-brew-install curl "bin/curl" || return 1

    PKG_HOME=$(ham-brew-installdir php@7.4)
    if [ ! -e "$PKG_HOME/bin/php" ]; then
      echo "I/Brew php@7.4 not found, install..."
      ham-brew tap shivammathur/php
      ham-brew-install shivammathur/php/php@7.4 "bin/php" php@7.4 || return 1
    fi
    pathenv_add "${PHP_HOME}/bin"
    pathenv_add "${HOME}/.composer/vendor/bin"
    pathenv_add "${HAM_TOOLSET_DIR}"
    export HAM_PHP_EXE_PATH="$PHP_HOME/bin/php"
    export HAM_PHP_FPM_EXE_PATH="$PHP_HOME/sbin/php-fpm"
    ;;
  LINUX*)
    if [ -z "$(where_inpath "php$HAM_PHP_VERSION")" ]; then
      echo "W/php 7.4 not found, trying to install with sudo..."
      (
        set -ex
        sudo apt-get install -y php7.4-common php7.4-mysql php7.4-xml php7.4-xmlrpc php7.4-curl php7.4-gd php7.4-imagick php7.4-cli php7.4-dev php7.4-imap php7.4-mbstring php7.4-opcache php7.4-pgsql php7.4-soap php7.4-sqlite3 php7.4-zip php7.4-intl php7.4-fpm
      ) || return 1
      if [ -z "$(where_inpath "php$HAM_PHP_VERSION")" ]; then
        echo "E/Can't find 'php$HAM_PHP_VERSION' after installation."
        return 1
      fi
    fi
    pathenv_add "${HAM_TOOLSET_DIR}"
    export HAM_PHP_EXE_PATH="/usr/bin/php$HAM_PHP_VERSION"
    export HAM_PHP_FPM_EXE_PATH="/usr/sbin/php-fpm$HAM_PHP_VERSION"
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

VER="--- php ------------------------"
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
