#!/bin/bash

# toolset
export HAM_TOOLSET=JAVA_JRE
export HAM_TOOLSET_VER=6
export HAM_TOOLSET_NAME=java_jre6
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/java_jre6

# path setup
case $HAM_OS in
    NT*)
        export JAVA_HOME=${HAM_TOOLSET_DIR}/nt-x86/
        export PATH=${JAVA_HOME}/bin:${PATH}
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
