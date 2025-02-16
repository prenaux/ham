#!/bin/bash -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
. "$HAM_HOME/bin/ham-bash-setenv.sh"
cd "$SCRIPT_DIR/src"

HAM_NO_VER_CHECK=1 . ham-toolset linux_x64 || exit 1

if [[ -e "./Makefile" ]]; then
  echo "I/Already configured"
else
  echo "I/Configure"
  (set -x ;
   chmod +x ./configure ;
   ./configure)
fi

echo "I/Create output unix folder"
(set -x ;
 mkdir -p bin.unix)

echo "I/Building jambase.c..."
(set -x ;
 gcc -o bin.unix/mkjambase mkjambase.c ;
 ./bin.unix/mkjambase jambase.c Jambase)

echo "I/Building Ham"
(set -x ;
 make ;
 mkdir -p "$HAM_HOME/bin/lin-x64" ;
 cp -f "./bin.unix/ham.x64" "$HAM_HOME/bin/lin-x64/ham")

echo "I/Cleaning up temporary build files"
(set -x ;
 make clean ;
 rm -f ham0 ;
 rm -Rf bin.unix/ ;
 rm -Rf ham0.dSYM/)

echo "I/Run tests"
(set -x ;
 "$SCRIPT_DIR/run-tests.sh")

echo "I/New ham version"
(set -x ;
 ham -v)

echo "I/Done."
