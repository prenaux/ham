#!/bin/bash

# toolset
export HAM_TOOLSET=HASKELL
export HAM_TOOLSET_NAME=haskell
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/haskell"

# path setup
case $HAM_OS in
    NT*)
        # Setup ghc
        toolset_check_and_dl_ver haskell nt-x86 v1 || return 1
        # Setup ghc
        GHC_BASE_DIR="${HAM_TOOLSET_DIR}/nt-x86"
        GHC_BIN_DIR="${GHC_BASE_DIR}/bin"
        # Setup the path
        export PATH="${GHC_BIN_DIR}:${GHC_BASE_DIR}/lib/bin:${GHC_BASE_DIR}/lib/extralibs/bin:${PATH}"

        # Setup cabal
        CABAL_DIR="${GHC_BASE_DIR}/cabal"
        CABAL_CONFIG="${CABAL_DIR}/config"
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
        ;;
    OSX*)
        ham-brew-install ghc@8.10 "bin/ghc"
        ham-brew-install cabal-install "bin/cabal"
        ham-brew-install haskell-stack "bin/stack"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# Version checks
VER="--- haskell ----------------------
GHC: `ghc --version`
CABAL: `cabal --version | grep cabal`
STACK: `stack --numeric-version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
