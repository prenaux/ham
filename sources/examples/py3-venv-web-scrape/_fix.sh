#!/bin/bash -e
export HAM_NO_VER_CHECK=1
if [[ -z "$HAM_HOME" ]]; then echo "E/HAM_HOME not set !"; exit 1; fi
. "$HAM_HOME/bin/ham-bash-setenv.sh"

toolset_import_once python_3

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"
source py3-venv-activate

#
# pyre can't lint a single file, only the folders specified in .pyre_configuration
#
# `pyre infer -i` updates the source with all the infered type annotations
#
(set -x ; pyre infer -i)

# TODO: ufmt can lint a single file, so we should do that when its specified
(set -x ; ufmt format "./src")
