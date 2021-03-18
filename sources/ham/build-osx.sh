#!/bin/bash -e
. ham-bash-lib.sh
. ham-toolset default
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

mkdir -p "$HAM_HOME/bin/$HAM_BIN_LOA"
echo == Copying output to "$HAM_HOME/bin/$HAM_BIN_LOA/ham"
cp -f ./bin.unix/ham "$HAM_HOME/bin/$HAM_BIN_LOA/ham"
errcheck $? build-ham-osx "Can't output ham to final directory"

echo == Cleaning up temporary build files
rm -f ham0
rm -Rf bin.unix/
rm -Rf ham0.dSYM/
