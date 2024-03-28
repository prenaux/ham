#!/bin/bash
if [ ! -d "$JAVA_HOME" ] || [ -z "$(where_inpath java)" ]; then
  case $HAM_OS in
    NT*)
      toolset_import_once java_jdk18 || return 1
      ;;
    OSX)
      toolset_import_once java_jdk_11 || return 1
      ;;
    LINUX)
      toolset_import_once java_jdk_21 || return 1
      ;;
    *)
      echo "E/Toolset: Unsupported host OS"
      return 1
      ;;
  esac
else
  echo "I/java_jdk already imported."
fi
