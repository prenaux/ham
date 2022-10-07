#!/bin/bash -e
. ham-bash-lib.sh
BIN_LOA=${HAM_TARGET_BIN_LOA:-$HAM_BIN_LOA}
case $BIN_LOA in
    osx-arm64)
        . ham-toolset macos_arm64 || return 1
        ;;
    osx-x64)
        . ham-toolset macos_x64 || return 1
        ;;
    *)
        echo "E/Toolset: Unsupported target '$BIN_LOA'."
        return 1
        ;;
esac
errcheck $? build-ham-osx "Can't setup toolset"
cd src

echo == Create output unix folder
mkdir -p bin.unix

echo == Building jambase.c...
gcc -o bin.unix/mkjambase mkjambase.c
./bin.unix/mkjambase jambase.c Jambase
errcheck $? build-ham-osx "Can't build jambase"

echo == Building Ham
make -f builds/Makefile.osx clean
make -f builds/Makefile.osx
errcheck $? build-ham-osx "Can't build ham"

mkdir -p "$HAM_HOME/bin/$BIN_LOA"
echo == Copying output to "$HAM_HOME/bin/$BIN_LOA/ham"
# Explicitly remove the file first otherwise macOS will quarantine the file
rm -f "$HAM_HOME/bin/$BIN_LOA/ham"
cp -f ./bin.unix/ham "$HAM_HOME/bin/$BIN_LOA/ham"
errcheck $? build-ham-osx "Can't output ham to final directory"

echo == Cleaning up temporary build files
rm -f ham0
rm -Rf bin.unix/
rm -Rf ham0.dSYM/
