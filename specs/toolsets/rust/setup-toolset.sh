#!/bin/bash

# toolset
export HAM_TOOLSET=RUST
export HAM_TOOLSET_NAME=rust
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/rust"

# path setup
case $HAM_OS in
  OSX* | LINUX*)
    if [ ! -e "$HOME/.cargo/env" ]; then
      echo "W/rustc not found, installing using rustup..."
      curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
      if [ ! -e "$HOME/.cargo/env" ]; then
        echo "E/Can't install rust."
        return 1
      fi
    fi
    # Sets PATH for us
    source "$HOME/.cargo/env"
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac
pathenv_add "${HAM_TOOLSET_DIR}"

# Version checks
VER="--- rust ----------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
rustc: $(rustc --version)
cargo: $(cargo --version)"; then
    echo "E/Can't get version."
    return 1
  fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
