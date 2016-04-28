#!/bin/bash
toolset_import gcc_470
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=CLANG
export HAM_TOOLSET_VER=33
export HAM_TOOLSET_NAME=clang_33
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}"

# path setup
case $HAM_OS in
    OSX)
        export RUN_DEBUGGER=lldb
        export RUN_DEBUGGER_PARAMS=-f
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- clang_33 ------------------------
`clang --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
