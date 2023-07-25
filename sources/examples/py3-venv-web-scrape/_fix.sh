#!/bin/bash
export HAM_NO_VER_CHECK=1
if [[ -z "$HAM_HOME" ]]; then echo "E/HAM_HOME not set !"; exit 1; fi
. "$HAM_HOME/bin/ham-bash-setenv.sh"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"
. hat ./_ham_project > /dev/null
set -ex

#
# pyre can't lint a single file, only the folders specified in .pyre_configuration
#
# `pyre infer -i` updates the source with all the infered type annotations
#
pyre infer -i

# TODO: ufmt can lint a single file, so we should do that when its specified
ufmt format "./src" "./tsrc"
