#!/bin/bash -e
#===== PRELUDE BEGIN ===========
if [[ -z "$HAM_HOME" ]]; then
  echo "E/HAM_HOME not set !"
  exit 1
fi
# shellcheck disable=SC2034
SCRIPT_NAME=$(basename "$0")
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
. "$HAM_HOME/bin/ham-bash-setenv.sh"
. "$HAM_HOME/bin/ham-lint-fix-lib.sh"
cd "$SCRIPT_DIR"
#===== PRELUDE END =============
ham_lint_sh "$@"
