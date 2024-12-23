#!/bin/bash
if [ "$1" == "force" ]; then
  shift
  if [ "$1" == "silent" ]; then
    shift
  else
    ALREADY_IMPORTED=$(ni-hget HAM_IMPORTS_TOOLSETS "$1")
    if [[ "$ALREADY_IMPORTED" = "1" ]]; then
      log_warning "ham-toolset-import.sh: toolset already imported '$1', force reimported."
    fi
  fi
else
  ALREADY_IMPORTED=$(ni-hget HAM_IMPORTS_TOOLSETS "$1")
  if [[ "$ALREADY_IMPORTED" = "1" ]]; then
    complain ham-toolset-import.sh "toolset already imported '$1'."
    return 1
  fi
fi

SETUP_SCRIPT=
FOUND_SETUP_SCRIPT=no

# Look in HAM_HOME/specs for the toolset definition
if [ "$FOUND_SETUP_SCRIPT" == "no" ]; then
  DIR="${HAM_HOME}/specs/toolsets/$1"
  SETUP_SCRIPT="$DIR/setup-toolset.sh"
  if [ -f "$SETUP_SCRIPT" ]; then
    FOUND_SETUP_SCRIPT="from GLOBAL."
  fi
fi

# Look in HAM_PROJECT_DIR/specs for the toolset definition, this is the
# directory of the current _ham_project
if [[ -d "${HAM_PROJECT_DIR}" && "$FOUND_SETUP_SCRIPT" == "no" ]]; then
  DIR="${HAM_PROJECT_DIR}/specs/toolsets/$1"
  SETUP_SCRIPT="$DIR/setup-toolset.sh"
  if [ -f "$SETUP_SCRIPT" ]; then
    FOUND_SETUP_SCRIPT="from PROJECT (${HAM_PROJECT_DIR})."
  fi
fi

if [ "$FOUND_SETUP_SCRIPT" == "no" ]; then
  complain ham-toolset-do-import.sh "Can't find the toolset '$1'"
  return 1
fi

export PATH=$PATH
# shellcheck disable=SC1090
if ! HAM_DIE_SHOULD_RETURN=yes source "$SETUP_SCRIPT"; then
  complain ham-toolset-do-import.sh "Toolset '$1' import failed !"
  return 1
else
  if [[ -z $HAM_IMPORTED_TOOLSETS ]]; then
    export HAM_IMPORTED_TOOLSETS="$1"
  else
    export HAM_IMPORTED_TOOLSETS="$HAM_IMPORTED_TOOLSETS $1"
  fi
  ni-hput HAM_IMPORTS_TOOLSETS "$1" 1
  log_info "Imported toolset '$1' ${FOUND_SETUP_SCRIPT}"
fi
