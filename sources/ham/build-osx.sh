#!/bin/bash -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
. "$HAM_HOME/bin/ham-bash-setenv.sh"
cd "$SCRIPT_DIR/src"

BIN_LOA=$(toolset_get_target_bin_loa)
case $BIN_LOA in
  osx-arm64)
    HAM_NO_VER_CHECK=1 . ham-toolset macos_arm64 || exit 1
    ;;
  osx-x64)
    HAM_NO_VER_CHECK=1 . ham-toolset macos_x64 || exit 1
    ;;
  *)
    echo "E/Toolset: Unsupported target '$BIN_LOA'."
    exit 1
    ;;
esac

echo "I/Create output unix folder"
(set -x ;
 mkdir -p bin.unix)

echo "I/Building jambase.c..."
(set -x ;
 clang -o bin.unix/mkjambase mkjambase.c ;
 ./bin.unix/mkjambase jambase.c Jambase)

echo "I/Building Ham"
(set -x ;
 make -f builds/Makefile.osx clean ;
 make -f builds/Makefile.osx)

echo "I/Copying output to '$HAM_HOME/bin/$BIN_LOA/ham'"
(set -x ;
 mkdir -p "$HAM_HOME/bin/$BIN_LOA" ;
 # Explicitly remove the file first otherwise macOS will quarantine the file
 rm -f "$HAM_HOME/bin/$BIN_LOA/ham" ;
 cp -f ./bin.unix/ham "$HAM_HOME/bin/$BIN_LOA/ham")

echo "I/Cleaning up temporary build files"
(set -x ;
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
