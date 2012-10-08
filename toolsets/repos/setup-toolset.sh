#!/bin/bash

# toolset
export HAM_TOOLSET_IS_SETUP_REPOS=1
export HAM_TOOLSET=REPOS
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=repos
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/repos

# path setup
case $HAM_OS in
    NT*)
        export REPOS_DIR=${HAM_TOOLSET_DIR}/nt-x86/
        export PATH=${HAM_TOOLSET_DIR}/:${PATH}:${REPOS_DIR}/git/bin/:${REPOS_DIR}/svn/bin/
        if [ ! -e "$REPOS_DIR" ]; then
            toolset_dl repos repos_nt-x86
            if [ ! -e "$REPOS_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- repos ------------------------
--- git ---
`git --version`
--- svn ---
`svn --version | grep 'Slik'`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
