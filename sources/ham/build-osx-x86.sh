#!/bin/bash
. ham-bash-lib.sh
. ham-toolset default
errcheck $? build-ham-osx-x86 "Can't setup toolset"

cd src

echo == Configure
if [[ ! -e ./Makefile ]]; then
    chmod +x ./configure
    ./configure
fi

echo == Building Ham
make clean
make
mkdir -p "$HAM_HOME/bin/osx-x86/"
cp -f ./bin.unix/ham.x86 "$HAM_HOME/bin/osx-x86/ham"

echo == Cleaning up temporary build files
rm -f ham0
rm -Rf bin.unix/
rm -Rf ham0.dSYM/
