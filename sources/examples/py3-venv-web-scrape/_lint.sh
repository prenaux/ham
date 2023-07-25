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
set -e ;

#
# pyre can't lint a single file, only the folders specified in
# .pyre_configuration
#
# the sed abomination is to strip the color code so that tools can parse the
# error output sanely without needing to use the json output
#
if [[ "${HAM_OS}" == "NT"* ]]; then
  log_warning "Pyre doesn't work on Windows, skipped."
else
  (log_info "Running pyre..."
   set +e ;
   set -x ;
   pyre 2>&1 | \
       sed -r "s/\x1B\[[0-9;]*[mG]//g" | \
       sed -r "s/tsrc\///g" | \
       sed -r "s/src\///g" ;
   errcode=${PIPESTATUS[0]} ;
   set +x ;
   errcheck "${errcode}" _lint "Pyre failed." || exit 1)
fi

# Strip comments from _flake8 and output to .flake8
sed -e '/^#[^!].*/d' -e 's/\(.*[^!]\)#.*[^}]/\1/' _flake8 > ./venv/_flake8_stripped
# Run flake8
(log_info "Running flake8..."
 set +e ;
 set -x ;
 flake8 --config ./venv/_flake8_stripped --color=never ./src/ ./tsrc/ | \
     sed -r "s/.\/tsrc\///g" | \
     sed -r "s/.\/src\///g" ;
 errcode=${PIPESTATUS[0]} ;
 set +x ;
 errcheck "${errcode}" _lint "Flake8 failed." || exit 1)

# TODO: ufmt can lint a single file, so we should do that when its specified
(log_info "Running ufmt..." ;
 set +e ;
 set -x ;
 ufmt check "./src" "./tsrc" ;
 errcode=${PIPESTATUS[0]} ;
 set +x ;
 errcheck "${errcode}" _lint "Ufmt failed." || exit 1)

log_info "Done." ;
