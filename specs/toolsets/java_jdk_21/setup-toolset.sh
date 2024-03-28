#!/bin/bash

# toolset
export HAM_TOOLSET=JAVA_JRE
export HAM_TOOLSET_NAME=java_jdk_21
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/java_jdk_21"

# path setup
case $HAM_OS in
  NT*)
    export JAVA_HOME="${HAM_TOOLSET_DIR}/nt-x86/"
    if [ ! -e "$JAVA_HOME/bin/java.exe" ] || [ ! -e "$JAVA_HOME/bin/javac.exe" ]; then
      toolset_check_and_dl_ver java_jdk_21 java_jdk_21_nt-x86 v21
      if [ ! -e "$JAVA_HOME/bin/java.exe" ] || [ ! -e "$JAVA_HOME/bin/javac.exe" ]; then
        echo "E/nt-x86 folder doesn't exist in the toolset"
        return 1
      fi
    fi
    pathenv_add "${JAVA_HOME}/bin"
    ;;
  OSX)
    if [ "$HAM_BIN_LOA" == "osx-arm64" ]; then
      echo "E/Toolset: Unsupported host OS, no Java 21 for osx-arm64."
      return 1
    else
      export JAVA_HOME="${HAM_TOOLSET_DIR}/osx-x64/"
      if [ ! -e "$JAVA_HOME/bin/java" ] || [ ! -e "$JAVA_HOME/bin/javac" ]; then
        toolset_check_and_dl_ver java_jdk_21 osx-x64 v21 || return 1
        if [ ! -e "$JAVA_HOME/bin/java" ] || [ ! -e "$JAVA_HOME/bin/javac" ]; then
          echo "E/osx-x64 can't install Java 21"
          return 1
        fi
      fi
      pathenv_add "${JAVA_HOME}/bin"
    fi
    ;;
  LINUX)
    export JAVA_HOME="${HAM_TOOLSET_DIR}/${HAM_BIN_LOA}/"
    if [ ! -e "$JAVA_HOME/bin/java" ] || [ ! -e "$JAVA_HOME/bin/javac" ]; then
      toolset_check_and_dl_ver java_jdk_21 lin-x64 v21
      if [ ! -e "$JAVA_HOME/bin/java" ] || [ ! -e "$JAVA_HOME/bin/javac" ]; then
        echo "E/nt-x86 folder doesn't exist in the toolset"
        return 1
      fi
    fi
    pathenv_add "${JAVA_HOME}/bin"
    chmod +x "$JAVA_HOME/bin/"*
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

VER="--- java_jdk_21 ------------------------"
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
