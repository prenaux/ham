#!/bin/bash
. ham-bash-lib.sh
. ham-toolset default
errcheck $? build-ham-lin-x64 "Can't setup toolset"

cd src

echo == Configure
if [[ ! -e ./Makefile ]]; then
    chmod +x ./configure
    ./configure
fi

echo == Create output unix folder
mkdir -p bin.unix

echo == Building jambase.c...
gcc -o bin.unix/mkjambase mkjambase.c
./bin.unix/mkjambase jambase.c jambase

echo == Building Ham
make clean
make
mkdir -p "$HAM_HOME/bin/lin-x64/"
cp -f ./bin.unix/ham.x64 "$HAM_HOME/bin/lin-x64/ham"

echo == Cleaning up temporary build files
rm -f ham0
rm -Rf bin.unix/
rm -Rf ham0.dSYM/
