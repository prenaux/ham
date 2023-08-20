#!/bin/bash
echo "Opening $1 at '$2'"
if [ ! -f "$RUN_DEBUGGER" ]; then
  RUN_DEBUGGER=$(toolset_find_msvc_devenv)
  export RUN_DEBUGGER
  if [ ! -f "$RUN_DEBUGGER" ]; then
    echo "E/Can't find RUN_DEBUGGER (devenv.exe)"
    return 1
  fi
fi
"${RUN_DEBUGGER}" -edit "$1" -command "Edit.GoTo $2" &
