#!/bin/bash
#
# This is a simple environment setup to start using ham:
#   source ./ham/_env.sh
#

# Don't `set -e` in a script that's sourced, otherwise will die as soon as any
# command returns an error code.
# set -e

SCRIPT_SOURCED=$((return 0 2>/dev/null) && echo yes || echo "")
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export HAM_HOME="$SCRIPT_DIR"
export WORK="$( cd "$SCRIPT_DIR/.." && pwd )"

if [ -z "$BASH_START_PATH" ]; then
   export BASH_START_PATH="$PATH"
fi

if [ -z "$EDITOR" ]; then
    export EDITOR=ham-editor
fi

# Set the ham environment
. "$HAM_HOME/bin/ham-bash-setenv.sh"

# Setup the tools that we want by default, just repos/git.
. hat repos
HAM_DIE_SHOULD_RETURN=$SCRIPT_SOURCED errcheck $? ham_env "Can't import toolsets." || return 1
