#!/bin/bash -e
#===== PRELUDE BEGIN ===========
export SCRIPT_NAME=$(basename "$0")
export SCRIPT_DIR=$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)
if [[ -z "$HAM_HOME" ]]; then echo "E/HAM_HOME not set !"; exit 1; fi
. "$HAM_HOME/bin/ham-bash-setenv.sh"
. "$HAM_HOME/bin/ham-lint-fix-lib.sh"
cd "$SCRIPT_DIR"
#===== PRELUDE END =============
ham_lint_sh "$@"
