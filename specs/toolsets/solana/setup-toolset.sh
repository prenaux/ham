#!/bin/bash

# toolset
export HAM_TOOLSET=SOLANA
export HAM_TOOLSET_NAME=solana
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/solana"

pathenv_add "${HAM_TOOLSET_DIR}"

if [ -z "$SOLANA_REQUIRED_VERSION" ]; then
  complain solana "SOLANA_REQUIRED_VERSION not exported"
  return 1
fi

# platform
case $HAM_OS in
  OSX* | LINUX*)
    export SOLANA_DATA_DIR=$HAM_TOOLSET_DIR/$HAM_BIN_LOA/$SOLANA_REQUIRED_VERSION
    export SOLANA_BIN_DIR=$SOLANA_DATA_DIR/active_release/bin
    if [ ! -x "$SOLANA_BIN_DIR/solana" ]; then
      echo "I/Solana command not found, installing..."
      solana-install "$SOLANA_REQUIRED_VERSION" --data-dir "$SOLANA_DATA_DIR"
    fi
    pathenv_add "$SOLANA_BIN_DIR"
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

# version check
VER="--- solana -----------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  VER="$VER
$(solana --version)"
  if [ $? != 0 ]; then
    echo "E/Can't get Solana version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
