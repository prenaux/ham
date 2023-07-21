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
        pathenv_add "${GHC_BIN_DIR}"
        pathenv_add "${GHC_BASE_DIR}/lib/bin"
        pathenv_add "${GHC_BASE_DIR}/lib/extralibs/bin"

        # Setup cabal
        CABAL_DIR="${GHC_BASE_DIR}/cabal"
        CABAL_CONFIG="${CABAL_DIR}/config"
        mkdir -p "${CABAL_DIR}"
        mkdir -p "${CABAL_DIR}/bin"
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
        pathenv_add "${CABAL_DIR}/bin"
        ;;
    OSX*)
        if [ "$HAM_BIN_LOA" == "osx-arm64" ]; then
            # hs-stack uses the system level ghc on M1 mac because stack fails
            # to install it by itself
            ham-brew-install ghc@8.10 "bin/ghc"
        fi
        ham-brew-install haskell-stack "bin/stack"
        export HS_STACK_BIN_DIR="${HOME}/.local/bin"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

pathenv_add "${HAM_TOOLSET_DIR}"
pathenv_add "${HS_STACK_BIN_DIR}"

# Version checks
VER="--- haskell ----------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
GHC: `hs-stack ghc -- --version`
STACK: `hs-stack --numeric-version`"
    if [ $? != 0 ]; then
        echo "E/Can't get version."
        return 1
    fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
