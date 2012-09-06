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
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
