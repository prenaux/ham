#!/bin/bash
. "$HAM_HOME/bin/ham-bash-lib.sh"
if test -e "$PWD/sources/_ham_project"; then
  echo "$PWD/sources"
else
  upsearch _ham_project
fi
