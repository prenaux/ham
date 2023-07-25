#!/bin/bash
export HAM_NO_VER_CHECK=1
if [[ -z "$HAM_HOME" ]]; then echo "E/HAM_HOME not set !"; exit 1; fi
. "$HAM_HOME/bin/ham-bash-setenv.sh"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"
. hat ./_ham_project > /dev/null
set -e

if [ -z "$1" ]; then
    echo "I/Running all tests"
    pytest
else
    echo "I/Running specific tests"
    pytest -k "$*"
fi
