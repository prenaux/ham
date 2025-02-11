#!/bin/bash
export HAM_TARGET_BIN_LOA=osx-x64
export BUILD_BIN_LOA=osx-x64
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
"$SCRIPT_DIR/build-osx.sh"
