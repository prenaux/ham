#!/bin/bash

# toolset
export HAM_TOOLSET=TEXTEDITOR
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=texteditor
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/texteditor"

# path setup
case $HAM_OS in
    NT*)
        export TEXTEDITOR_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH="${HAM_TOOLSET_DIR}/":${PATH}
        if [ ! -e "$TEXTEDITOR_DIR" ]; then
            toolset_dl texteditor texteditor_nt-x86
            if [ ! -e "$TEXTEDITOR_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- texteditor ------------------------
Text Editor"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
