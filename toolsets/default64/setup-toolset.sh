#!/bin/bash

. ham-toolset-import.sh xslt_tools
if [ $? != 0 ]; then return 1; fi

# path setup
case $HAM_OS in
    NT*)
        . ham-toolset-import.sh msvc_11_x64
        ;;
    OSX*)
        . ham-toolset-import.sh clang_33
        export OSPLAT=X64
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
