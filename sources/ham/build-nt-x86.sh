#!/bin/bash
. ham-bash-lib.sh
. ham-toolset msvc_19_x86
errcheck $? build-ham-nt-x86 "Can't setup msvc_19_x86 toolset"

cd src
echo == Requires the VC DevEnv
mkdir -p bin.ntx86
echo == Building jambase.c...
cl -nologo mkjambase.c

echo == Building Ham
rm -f ham0 # Make sure any unix ham0 is cleaned up
./mkjambase.exe jambase.c jambase
nmake -f builds/win32-visualc.mk
errcheck $? build-ham-nt-x86 "Ham build failed"

cp bin.ntx86/ham.exe "${HAM_HOME}/bin/nt-x86/ham.exe"
errcheck $? build-ham-nt-x86 "Ham copy failed"

echo == Cleaning up temporary build files
rm -f *.obj *.lib *.exe ham ham0 *.pdb
rm -Rf bin.ntx86/
