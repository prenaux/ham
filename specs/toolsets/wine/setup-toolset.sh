#!/bin/bash

# toolset
export HAM_TOOLSET=wine
export HAM_TOOLSET_VER=4
export HAM_TOOLSET_NAME=wine
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/wine"

# path setup
case $HAM_OS in
    NT*)
        echo "E/Toolset: Unsupported host OS, you shouldn't need wine on Windows."
        ;;
    OSX*)
        # We use Wine 4 because Wine 5 uses 5x more disk space than Wine 4
        export WINE_USR_DIR="${HAM_TOOLSET_DIR}/osx-x64/wine_4/usr"
        if [ ! -e "$WINE_USR_DIR/bin/wine64" ]; then
            toolset_check_and_dl_ver wine osx-x64 v4|| return 1
            echo "I/Making sure that Wine isn't quarantined..."
            sudo xattr -r -d com.apple.quarantine "${WINE_USR_DIR}/bin"
            sudo xattr -r -d com.apple.quarantine "${WINE_USR_DIR}/lib"
            sudo xattr -r -d com.apple.quarantine "${WINE_USR_DIR}/lib64"
        fi
        export WINE_PREFIX_DIR="${HAM_TOOLSET_DIR}/osx-x64/wine-prefix"
        export WINEPREFIX="$WINE_PREFIX_DIR"
        export WINEARCH=win64
        # Should be done in the script launcher of the executable
        # export WINEDEBUG=-all
        # Commented because it leads to "warning: setlocale: LC_ALL: cannot
        # change locale (en_EN.UTF-8): No such file or directory"
        # export LC_ALL=en_EN.UTF-8
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# path
export PATH="${HAM_TOOLSET_DIR}":${PATH}

# version
VER="--- wine ------------------------------
Wine: $WINE_USR_DIR"
if [ $? != 0 ]; then
    echo "E/Can't get nicgc version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
