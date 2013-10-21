#!/bin/bash

# toolset
export HAM_TOOLSET=REPOS
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=repos
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/repos

export GIT_EXTENSIONS=${HAM_TOOLSET_DIR}/GitExtensions

# path setup
case $HAM_OS in
    NT*)
        export REPOS_DIR=${HAM_TOOLSET_DIR}/nt-x86
        export PATH=${HAM_TOOLSET_DIR}:${PATH}:${REPOS_DIR}/bin/:${REPOS_DIR}/git/bin/:${REPOS_DIR}/svn/bin/:${REPOS_DIR}/hg:${GIT_EXTENSIONS}
        if [ ! -e "$REPOS_DIR" ]; then
            toolset_dl repos repos_nt-x86
            if [ ! -e "$REPOS_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    OSX*)
        export PATH=${HAM_TOOLSET_DIR}:${PATH}
        chmod +x "$HAM_TOOLSET_DIR/repos"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

if [ ! -e $GIT_EXTENSIONS ]; then
    toolset_dl repos repos_GitExt
fi

VER="--- repos ------------------------
--- git ---
`git --version`"
if [ $? != 0 ]; then
    echo "E/Can't get Git version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

VER="--- mercurial ---
`hg --version | grep 'Mercurial'`"
if [ $? != 0 ]; then
    echo "E/Can't get Mercurial version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

VER="--- svn ---
`svn --version | grep 'svn,'`"
if [ $? != 0 ]; then
    echo "E/Can't get SVN version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
