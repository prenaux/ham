#!/bin/bash
export HAM_NO_VER_CHECK=1
if [[ -z "$HAM_HOME" ]]; then echo "E/HAM_HOME not set !"; exit 1; fi
. "$HAM_HOME/bin/ham-bash-setenv.sh"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"
if [ -z "$PY3_VENV_BIN_DIR" ]; then
  log_info "Importing _ham_project"
  . hat ./_ham_project > /dev/null
fi
set -ex

#
# pyre can't lint a single file, only the folders specified in .pyre_configuration
#
# `pyre infer -i` updates the source with all the infered type annotations
#
if [[ "${HAM_OS}" == "NT"* ]]; then
  log_warning "Pyre doesn't work on Windows, skipped."
else
  pyre infer -i
fi

# TODO: ufmt can lint a single file, so we should do that when its specified
ufmt format "./src" "./tsrc"
