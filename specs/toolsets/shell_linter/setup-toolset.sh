#!/bin/bash

# toolset
export HAM_TOOLSET=SHELL_LINTER
export HAM_TOOLSET_NAME=shell_linter
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/shell_linter"

# path setup
case $HAM_OS in
    OSX*)
        toolset_check_and_dl_ver shell_linter ${HAM_BIN_LOA} v1 || return 1
        export HAM_SHELL_LINTER_DIR="${HAM_TOOLSET_DIR}/${HAM_BIN_LOA}"
        pathenv_add "${HAM_SHELL_LINTER_DIR}"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS '$HAM_OS'"
        return 1
        ;;
esac

# path
pathenv_add "${HAM_TOOLSET_DIR}"

# version
VER="--- shell_linter ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
$(shellcheck --version | grep "version:")
$(shfmt --version)"
    if [ $? != 0 ]; then
      echo "E/Can't get shell_linter version."
      return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"