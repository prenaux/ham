#!/bin/bash

. ham-toolset-import.sh xslt_tools
if [ $? != 0 ]; then return 1; fi
toolset_import gcc_470
if [ $? != 0 ]; then return 1; fi

case $HAM_OS in
    LINUX)
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

export OSPLAT=X64
export BUILD_BIN_LOA=lin-x64
