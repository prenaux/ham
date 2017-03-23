#!/bin/bash

# toolset
export HAM_TOOLSET=HASKELL
export HAM_TOOLSET_VER=7
export HAM_TOOLSET_NAME=haskell
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/haskell"

# path setup
case $HAM_OS in
    OSX*)
        # Add GHC 7.10.3 to the PATH, via https://ghcformacosx.github.io/
        export GHC_DOT_APP="${HAM_TOOLSET_DIR}/osx-x86/ghc.app"
        export GHC_BASE_DIR="${GHC_DOT_APP}/Contents/"
        export GHC_BIN_DIR="${GHC_DOT_APP}/Contents/bin"
        if [ ! -e "${GHC_BIN_DIR}/ghc" ]; then
            toolset_dl haskell haskell_osx-x86
            if [ ! -e "${GHC_BIN_DIR}/ghc" ]; then
                echo "E/osx-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        export PATH="${HOME}/.local/bin:${HOME}/.cabal/bin:${GHC_BIN_DIR}:${PATH}"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

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
