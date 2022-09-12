#!/bin/bash
toolset_import nodejs || return 1
toolset_import ruby || return 1

# db setup
toolset_import postgres || return 1

# toolset
export HAM_TOOLSET=RAILS
export HAM_TOOLSET_NAME=rails
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/rails"

# path setup
case $HAM_OS in
    NT*)
        echo "E/Toolset: Forget Rails on Windows, it just doesnt work."
        return 1
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- rails ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`rails --version`"
    if [ $? != 0 ]; then
      echo "E/Can't get version."
      return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
