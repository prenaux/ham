#!/bin/bash

# toolset
export HAM_TOOLSET=JAVA_JDK
export HAM_TOOLSET_NAME=java_jdk_17
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/java_jdk_17"

# path setup
case $HAM_OS in
  OSX)
    BREW_DIR=$(ham-brew-installdir . prefix)
    export JAVA_HOME="$BREW_DIR/opt/openjdk@17/libexec/openjdk.jdk/Contents/Home"
    if [ ! -e "$JAVA_HOME/bin/java" ]; then
      echo "W/Couldn't find openjdk@17's java, trying to install with brew"
      ham-brew install openjdk@17
    fi
    pathenv_add "${JAVA_HOME}/bin"
    if [ -d "/usr/local/java-runtime/impl" ] && [ ! -e "/usr/local/java-runtime/impl/17" ]; then
      sudo ln -s "$JAVA_HOME" "/usr/local/java-runtime/impl/17"
    fi
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

VER="--- java_jdk_17 ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
--- java ---
$(java -version 2>&1)
--- javac ---
$(javac -version 2>&1)"; then
    echo "E/Can't get Java version."
    return 1
  fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
