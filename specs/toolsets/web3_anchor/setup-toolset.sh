#!/bin/bash
toolset_import_once rust || return 1

# toolset
export HAM_TOOLSET=WEB3_ANCHOR
export HAM_TOOLSET_NAME=web3_anchor
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/web3_anchor"
pathenv_add ${HAM_TOOLSET_DIR}

if [ -z "$ANCHOR_REQUIRED_VERSION" ]; then
  complain web3_anchor "ANCHOR_REQUIRED_VERSION not exported."
  return 1
fi

if [ -z $(where_inpath avm) -o -z $(where_inpath anchor) ]; then
  echo "I/Installing avm&anchor with cargo..."
  case $HAM_OS in
    LINUX*)
      (
        set -ex
        sudo apt-get -y install pkg-config libssl-dev
      )
      ;;
  esac
  (
    set -ex
    rustup update
    cargo install --git https://github.com/project-serum/anchor avm --locked
    avm install $ANCHOR_REQUIRED_VERSION
    avm use $ANCHOR_REQUIRED_VERSION
  ) || return 1
fi

ANCHOR_CURRENT_VERSION=$(anchor --version)
if [ "$ANCHOR_CURRENT_VERSION" != "anchor-cli $ANCHOR_REQUIRED_VERSION" ]; then
  echo "I/Switching to version to Anchor $ANCHOR_REQUIRED_VERSION"
  (
    set -ex
    rustup update
    avm install $ANCHOR_REQUIRED_VERSION
    avm use $ANCHOR_REQUIRED_VERSION
  ) || return 1
fi

# version check
VER="--- web3_anchor ------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  VER="$VER
$(anchor --version)"
  if [ $? != 0 ]; then
    echo "E/Can't get Anchor version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
