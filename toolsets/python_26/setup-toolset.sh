#!/bin/bash

# toolset
export HAM_TOOLSET=PYTHON
export HAM_TOOLSET_VER=26
export HAM_TOOLSET_NAME=python_26
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/python_26

# path setup
case $HAM_OS in
    NT*)
        export PYTHON_DIR=${HAM_TOOLSET_DIR}/nt-x86/
        export PATH=${PYTHON_DIR}:${PYTHON_DIR}/DLLs:${PATH}
        if [ ! -e "$PYTHON_DIR" ]; then
            toolset_dl python_26 python_26_nt-x86
            if [ ! -e "$PYTHON_DIR" ]; then
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

VER="--- python_26 ------------------------
`python --version 2>&1`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
