#!/bin/bash
export BUILD_BIN_LOA=osx-arm64
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
"$SCRIPT_DIR/build-osx.sh"
