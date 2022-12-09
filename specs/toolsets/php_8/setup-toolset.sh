#!/bin/bash

# toolset
export HAM_TOOLSET=PHP
export HAM_TOOLSET_NAME=php_8
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/php_8"

# path setup
case $HAM_OS in
    OSX*)
        export PHP_HOME=`ham-brew-installdir php@8.1`
        # freetds, a dependency of php@8.1, needs brew's curl and for some
        # reason complains about it and fail the install instead of having it
        # as a dependency...
        ham-brew-install curl "bin/curl" || return 1
        ham-brew-install php@8.1 "bin/php" || return 1
        export PATH="${HAM_TOOLSET_DIR}":"${HAM_HOME}/toolsets/php":"${HOME}/.composer/vendor/bin":"${PHP_HOME}/bin":${PATH}
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- php_8 ----------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`php --version`
`composer -V`"
    if [ $? != 0 ]; then
      echo "E/Can't get version."
      return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
