#!/bin/bash

# toolset
export HAM_TOOLSET=JAVA_JDK
export HAM_TOOLSET_VER=11
export HAM_TOOLSET_NAME=java_jdk_11
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/java_jdk_11"

# path setup
case $HAM_OS in
    OSX)
        if [ "$HAM_BIN_LOA" == "osx-arm64" ]; then
            export JAVA_HOME="/opt/homebrew/opt/openjdk@11/libexec/openjdk.jdk/Contents/Home/"
            if [ ! -e "$JAVA_HOME/bin/java" ]; then
                echo "W/Couldn't find openjdk@11's java, trying to install with brew"
                brew install openjdk@11
            fi
            export PATH="${JAVA_HOME}/bin":${PATH}
        else
            if [ ! -d "/usr/local/Cellar/openjdk@11/" ]; then
                echo "W/Couldn't find openjdk@11's java, trying to install with brew"
                brew install openjdk@11
                if [ ! -d "/usr/local/Cellar/openjdk@11/" ]; then
                    echo "F/Couldn't find openjdk@11's java even after installing"
                    return 1
                fi
            fi
            JAVA_11_BREW_VER=`ls /usr/local/Cellar/openjdk@11/`
            echo "I/JAVA_11_BREW_VER: $JAVA_11_BREW_VER"
            export JAVA_HOME="/usr/local/Cellar/openjdk@11/$JAVA_11_BREW_VER/"
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- java_jdk_11 ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
--- java ---
`java -version 2>&1`
--- javac ---
`javac -version 2>&1`"
    if [ $? != 0 ]; then
        echo "E/Can't get Java version."
        return 1
    fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
