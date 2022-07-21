#!/bin/bash

# toolset
export HAM_TOOLSET=SOLANA
export HAM_TOOLSET_NAME=solana
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/solana"

export PATH=${HAM_TOOLSET_DIR}:${PATH}

if [ -z "$SOLANA_REQUIRED_VERSION" ]; then
    complain solana "SOLANA_REQUIRED_VERSION not exported"
    return 1
fi


# platform
case $HAM_OS in
    OSX*|LINUX*)
        export SOLANA_DATA_DIR=$HAM_TOOLSET_DIR/$HAM_BIN_LOA
        export SOLANA_BIN_DIR=$SOLANA_DATA_DIR/active_release/bin
        export PATH="$SOLANA_BIN_DIR":"$HAM_TOOLSET_DIR":$PATH
        if [ ! -x "$SOLANA_BIN_DIR/solana" ]; then
            echo "I/Solana command not found, installing..."
            solana-install "$SOLANA_REQUIRED_VERSION" --data-dir "$SOLANA_DATA_DIR"
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# version check
VER="--- solana -----------------------
`solana --version`"
if [ $? != 0 ]; then
    echo "E/Can't get Solana version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
