#!/bin/bash

# toolset
export HAM_TOOLSET=JAVA_JRE
export HAM_TOOLSET_VER=6
export HAM_TOOLSET_NAME=java_jdk16
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/java_jdk16

# path setup
case $HAM_OS in
    NT*)
        export JAVA_HOME=${HAM_TOOLSET_DIR}/nt-x86/
        export PATH=${JAVA_HOME}/bin:${PATH}
        if [ ! -e "$JAVA_HOME/bin/java.exe" -o ! -e "$JAVA_HOME/bin/javac.exe" ]; then
            toolset_dl java_jdk16 java_jdk16_nt-x86
            if [ ! -e "$JAVA_HOME/bin/java.exe" -o ! -e "$JAVA_HOME/bin/javac.exe" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    OSX)
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- java_jdk16 ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
--- java ---
`java -version 2>&1`
--- javac ---
`javac -version 2>&1`"
    if [ $? != 0 ]; then
        echo "E/Can't get version."
        return 1
    fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
