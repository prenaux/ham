#!/bin/bash -e
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR/src"
HAM_NO_VER_CHECK=1 . "$SCRIPT_DIR/../../_env.sh"
HAM_NO_VER_CHECK=1 . ham-toolset msvc_19_x86
errcheck $? build-ham-nt-x86 "Can't setup msvc_19_x86 toolset"

echo "I/Create output ntx86 folder"
(set -x ;
 mkdir -p bin.ntx86)

echo "I/Building jambase.c..."
(set -x ;
 "${MSVCDIR_BIN}/cl.exe" -nologo mkjambase.c ;
 ./mkjambase.exe jambase.c Jambase)

echo "I/Building Ham"
(set -x ;
 # Make sure any unix ham0 is cleaned up
 rm -f ham0 ;
 nmake -f builds/win32-visualc.mk ;
 cp bin.ntx86/ham.exe "${HAM_HOME}/bin/nt-x86/ham.exe")

echo "I/Cleaning up temporary build files"
(set -x ;
 rm -f *.obj *.lib *.exe ham ham0 *.pdb ;
 rm -Rf bin.ntx86/)

echo "I/Done."
