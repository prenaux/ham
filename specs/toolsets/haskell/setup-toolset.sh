#!/bin/bash

# toolset
export HAM_TOOLSET=HASKELL
export HAM_TOOLSET_VER=7
export HAM_TOOLSET_NAME=haskell
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/haskell"

# path setup
case $HAM_OS in
    NT*)
        # Setup ghc
        toolset_check_and_dl_ver haskell nt-x86 v1 || return 1
        # Setup ghc
        export GHC_BASE_DIR="${HAM_TOOLSET_DIR}/nt-x86"
        export GHC_BIN_DIR="${GHC_BASE_DIR}/bin"
        # Setup the path
        export PATH="${GHC_BIN_DIR}:${GHC_BASE_DIR}/lib/bin:${GHC_BASE_DIR}/lib/extralibs/bin:${PATH}"
        ;;
    OSX*)
        # Add GHC 7.10.3 to the PATH, via https://ghcformacosx.github.io/
        export GHC_BASE_DIR="${HAM_TOOLSET_DIR}/osx-x86/"
        export GHC_DOT_APP="${HAM_TOOLSET_DIR}/osx-x86/ghc.app"
        export GHC_BIN_DIR="${GHC_DOT_APP}/Contents/bin"
        if [ ! -e "${GHC_BIN_DIR}/ghc" ]; then
            toolset_dl haskell haskell_osx-x86
            if [ ! -e "${GHC_BIN_DIR}/ghc" ]; then
                echo "E/osx-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        export PATH="${GHC_BIN_DIR}:${PATH}"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# Setup cabal
export CABAL_DIR="${GHC_BASE_DIR}/cabal"
export CABAL_CONFIG="${CABAL_DIR}/config"
mkdir -p "${CABAL_DIR}"
mkdir -p "${CABAL_DIR}/packages"
mkdir -p "${CABAL_DIR}/world"
mkdir -p "${CABAL_DIR}/logs"
CABAL_DIR_NATIVE="`nativedir "${CABAL_DIR}"`"
# Write cabal config
echo "remote-repo-cache: ${CABAL_DIR_NATIVE}/packages" > "${CABAL_CONFIG}"
echo "world-file: ${CABAL_DIR_NATIVE}/world" >> "${CABAL_CONFIG}"
echo "build-summary: ${CABAL_DIR_NATIVE}/logs/build.log" >> "${CABAL_CONFIG}"
echo "symlink-bindir: ${CABAL_DIR_NATIVE}/bin" >> "${CABAL_CONFIG}"
echo "repository hackage.haskell.org" >> "${CABAL_CONFIG}"
echo "  url: http://hackage.haskell.org/" >> "${CABAL_CONFIG}"
export PATH="${CABAL_DIR}/bin:${PATH}"

# Version checks
VER="--- haskell ----------------------
GHC: `ghc --version`
CABAL: `cabal --version | grep cabal`
STACK: `stack --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
