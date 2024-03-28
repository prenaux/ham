#!/bin/bash

# toolset
export HAM_TOOLSET=JAVA_JRE
export HAM_TOOLSET_NAME=java_jdk_21
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/java_jdk_21"

# path setup
case $HAM_OS in
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
