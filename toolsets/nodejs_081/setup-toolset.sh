#!/bin/bash

# toolset
export HAM_TOOLSET=NODEJS
export HAM_TOOLSET_VER=081
export HAM_TOOLSET_NAME=nodejs_081
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/nodejs_081

# path setup
case $HAM_OS in
    NT*)
        export NODEJS_DIR=${HAM_TOOLSET_DIR}/nt-x86/
        export PATH=${NODEJS_DIR}:${PATH}
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
