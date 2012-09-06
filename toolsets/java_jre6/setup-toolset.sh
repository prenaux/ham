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
        if [ ! -e "$JAVA_HOME/bin/java.exe" ]; then
            toolset_dl java_jre6 java_jre6_nt-x86
            if [ ! -e "$JAVA_HOME/bin/java.exe" ]; then
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

VER="--- java_jre6 ------------------------
`java -version 2>&1`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
