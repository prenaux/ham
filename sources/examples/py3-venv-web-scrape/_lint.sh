#!/bin/bash -e
export HAM_NO_VER_CHECK=1
if [[ -z "$HAM_HOME" ]]; then echo "E/HAM_HOME not set !"; exit 1; fi
. "$HAM_HOME/bin/ham-bash-setenv.sh"

toolset_import_once python_3

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"
source py3-venv-activate

#
# pyre can't lint a single file, only the folders specified in
# .pyre_configuration
#
# the sed abomination is to strip the color code so that tools can parse the
# error output sanely without needing to use the json output
#
(set -x ; pyre 2>&1 | sed -r "s/\x1B\[[0-9;]*[mG]//g" | sed -r "s/src\///g")

# Strip comments from _flake8 and output to .flake8
sed -e '/^#[^!].*/d' -e 's/\(.*[^!]\)#.*[^}]/\1/' _flake8 > ./venv/_flake8_stripped
# Run flake8
(set -x ;
 flake8 --config ./venv/_flake8_stripped --color=never ./src/ | sed -r "s/.\/src\///g")

# TODO: ufmt can lint a single file, so we should do that when its specified
(set -x ; ufmt check "./src")
