#!/bin/bash

# toolset
export HAM_TOOLSET=HADOOP
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=hadoop
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/hadoop"

# path setup
case $HAM_OS in
    OSX*)
        export HADOOP_HOME="${HAM_TOOLSET_DIR}/jvm/"
        export PATH="${HADOOP_HOME}/bin":${PATH}
        if [ ! -e "$HADOOP_HOME" ]; then
            toolset_dl hadoop hadoop_jvm
            if [ ! -e "$HADOOP_HOME" ]; then
                echo "E/jvm folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# path
export PATH="${HAM_TOOLSET_DIR}":${PATH}

VER="--- hadoop ---------------------
`hadoop version | grep Hadoop`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
