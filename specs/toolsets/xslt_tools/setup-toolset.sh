#!/bin/bash
toolset_import_once java_jdk || return 1

# toolset
export HAM_TOOLSET=XSLT_TOOLS
export HAM_TOOLSET_NAME=xslt_tools
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/xslt_tools"

# path setup
case $HAM_OS in
  NT*)
    export XSLT_TOOLS_BIN_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
    if [ ! -e "$XSLT_TOOLS_BIN_DIR" ]; then
      toolset_dl xslt_tools xslt_tools_1_nt-x86
      if [ ! -e "$XSLT_TOOLS_BIN_DIR" ]; then
        echo "E/nt-x86 folder doesn't exist in the toolset"
        return 1
      fi
    fi
    pathenv_add "${XSLT_TOOLS_BIN_DIR}"
    ;;
  LINUX | OSX)
    # xsltproc is already bundled in OSX
    # Make sure all the xslt_tools scripts are executable
    chmod +x "${HAM_TOOLSET_DIR}/xslt_tools-"*
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

# jars
export XSLT_TOOLS_JARS_DIR="${HAM_TOOLSET_DIR}/jars/"
if [ ! -e "$XSLT_TOOLS_JARS_DIR" ]; then
  toolset_dl xslt_tools xslt_tools_1_jars
  if [ ! -e "$XSLT_TOOLS_JARS_DIR" ]; then
    echo "E/jars folder doesn't exist in the toolset"
    return 1
  fi
fi

# path
pathenv_add "${HAM_TOOLSET_DIR}"

# version
VER="--- xslt_tools ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
--- xsltproc ---
$(xsltproc --version | grep xsltproc)"; then
    echo "E/Can't get xsltproc version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
--- saxon ------
$(xslt_tools-saxon -version)"; then
    echo "E/Can't get xslt_tools-saxon version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
