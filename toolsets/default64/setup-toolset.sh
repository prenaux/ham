#!/bin/bash

. ham-toolset-import.sh xslt_tools
if [ $? != 0 ]; then return 1; fi

# path setup
case $HAM_OS in
    NT*)
        . ham-toolset-import.sh msvc_13_x64
        ;;
    OSX*)
        . ham-toolset-import.sh clang_33
        export OSPLAT=X64
        export BUILD_BIN_LOA=osx-x64
        ;;
    LINUX*)
        . ham-toolset-import.sh gcc_470
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
