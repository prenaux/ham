#!/bin/bash

# toolset
export HAM_TOOLSET=REPOS
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=repos
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/repos"

# path setup
case $HAM_OS in
    NT*)
        export REPOS_DIR="${HAM_TOOLSET_DIR}/nt-x86"
        export PATH=${HAM_TOOLSET_DIR}:${PATH}:${REPOS_DIR}/bin/:${REPOS_DIR}/git/bin/:${REPOS_DIR}/svn/bin/:${REPOS_DIR}/hg
        if [ ! -e "${REPOS_DIR}/git/bin/git.exe" ]; then
            toolset_dl repos repos_nt-x86
            if [ ! -e "${REPOS_DIR}/git/bin/git.exe" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        export OPENSSL_CONF="${REPOS_DIR}/git/ssl/openssl.cnf"
        ;;
    OSX*)
        export PATH=${HAM_TOOLSET_DIR}:${PATH}
        chmod +x "$HAM_TOOLSET_DIR/repos"
        ;;
    LINUX*)
        export PATH=${HAM_TOOLSET_DIR}:${PATH}
        chmod +x "$HAM_TOOLSET_DIR/repos"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- repos ------------------------
--- git ---
`git --version`"
if [ $? != 0 ]; then
    echo "E/Can't get Git version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

HG_PATH=`where_inpath hg`
if [ -e "$HG_PATH" ]; then
    VER="--- mercurial ---
`hg --version | grep 'Mercurial'`"
    if [ $? != 0 ]; then
        echo "E/Can't get Mercurial version."
        return 1
    fi
else
    VER="--- mercurial ---
W/Mercurial is not installed or not accessible from the PATH !"
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

SVN_PATH=`where_inpath svn`
if [ -e "$SVN_PATH" ]; then
    VER="--- svn ---
`svn --version | grep 'svn,'`"
    if [ $? != 0 ]; then
        echo "E/Can't get SVN version."
        return 1
    fi
else
    VER="--- svn ---
W/SVN is not installed or not accessible from the PATH !"
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
