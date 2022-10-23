#!/bin/bash -e
#
# This is a simple environment setup to start using ham:
#   source ./ham/_env.sh
#
SCRIPT_SOURCED=$((return 0 2>/dev/null) && echo yes || echo "")
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
if [ -z "$SCRIPT_SOURCED" ]; then
  echo "W/Should only be used while sourced."
fi

export HAM_HOME="$SCRIPT_DIR"
export WORK="$( cd "$SCRIPT_DIR/.." && pwd )"

#
# You dont want BASH_START_PATH since it will leak your local machine's
# environment into ham. It is there as an escape hatch to include user only
# scripts used for work, only use when you know the consequences - your CI and
# prod machine generally won't have this.
#
# if [ -z "$BASH_START_PATH" ]; then
#    export BASH_START_PATH="$PATH"
# fi

if [ -z "$EDITOR" ]; then
    export EDITOR=ham-editor
fi

# Set the ham environment
. "$HAM_HOME/bin/ham-bash-setenv.sh"

# Setup the tools that we want by default, just repos/git.
toolset_import_list repos || return 1
