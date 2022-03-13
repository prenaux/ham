#!/bin/bash

# toolset
export HAM_TOOLSET=SVN
export HAM_TOOLSET_NAME=svn
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/svn"

export PATH=${HAM_TOOLSET_DIR}:${PATH}

# platform
case $HAM_OS in
    OSX*)
        ham-brew-install svn "bin/svn"
        ;;
    LINUX*)
        if [ -z `which svn` ]; then
            sudo apt install -y subversion
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# version check
VER="--- svn --------------------------
`svn --version | grep ", version"`"
if [ $? != 0 ]; then
    echo "E/Can't get SVN version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
