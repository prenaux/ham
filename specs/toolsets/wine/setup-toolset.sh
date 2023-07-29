#!/bin/bash

# toolset
export HAM_TOOLSET=wine
export HAM_TOOLSET_NAME=wine
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/wine"

# path setup
case $HAM_OS in
    NT*)
        echo "E/Toolset: Unsupported host OS, you shouldn't need wine on Windows."
        ;;
    OSX*)
        export WINE_USR_DIR="${HAM_TOOLSET_DIR}/osx-x64/wine_7"
        if [ ! -e "$WINE_USR_DIR/bin/wine64" ]; then
            toolset_check_and_dl_ver wine osx-x64 v7_0 || return 1
        fi
        export WINE_PREFIX_DIR="${HAM_TOOLSET_DIR}/osx-x64/wine-prefix"
        export WINEPREFIX="$WINE_PREFIX_DIR"
        export WINEARCH=win64
        # Should be done in the script launcher of the executable, remove all the debug output
        # export WINEDEBUG=-all
        # Commented because it leads to "warning: setlocale: LC_ALL: cannot
        # change locale (en_EN.UTF-8): No such file or directory"
        # export LC_ALL=en_EN.UTF-8
        ;;
    LINUX*)
        if [ -z `where_inpath wine` ]; then
          (set -ex ;
           sudo apt -y update ;
           sudo apt install -y wine)
          if [ -z `where_inpath wine` ]; then
            echo "E/Can't find wine after installing it."
            exit 1
          fi
        fi
        export WINEARCH=win64
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# path
pathenv_add "${HAM_TOOLSET_DIR}"

# version
VER="--- wine ------------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`$WINE_USR_DIR/bin/wine64 --version`
Wine: $WINE_USR_DIR
Wine Prefix: $WINEPREFIX"
    if [ $? != 0 ]; then
      echo "E/Can't get nicgc version."
      return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
