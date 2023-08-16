#!/bin/bash -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"
. "$HAM_HOME/bin/ham-bash-setenv.sh"
export HAM_NO_VER_CHECK=1
. hat >/dev/null
set -ex

ham Run_ham-test-thrift-server
