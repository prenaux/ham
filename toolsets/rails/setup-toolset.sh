#!/bin/bash
toolset_import nodejs || return 1
toolset_import ruby || return 1

# db setup
toolset_import postgres || return 1

# toolset
export HAM_TOOLSET=RAILS
export HAM_TOOLSET_VER=420
export HAM_TOOLSET_NAME=rails
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/rails"

# path setup
case $HAM_OS in
    OSX*)
        ;;
    NT*)
        echo "E/Toolset: Forget Rails on Windows, it just doesnt work."
        return 1
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- rails ------------------------
`rails --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
