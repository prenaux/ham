#!/bin/bash
. "$HAM_HOME/bin/ham-bash-lib.sh"
if test -e "$PWD/sources/_build.ham"; then
  echo "$PWD/sources"
else
  upsearch _build.ham
fi
