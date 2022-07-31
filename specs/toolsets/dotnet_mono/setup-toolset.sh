#!/bin/bash

# toolset
export HAM_TOOLSET=DOTNET_MONO
export HAM_TOOLSET_NAME=dotnet_mono
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/dotnet_mono"

# path setup
case $HAM_OS in
    OSX*)
        # osx-x64 works on osx-arm64 aswell, its a universal binary
        # toolset_check_and_dl_ver dotnet_mono osx-x64 v6_12 || return 1
        export MONO_HOME="${HAM_TOOLSET_DIR}/$HAM_BIN_LOA"
        export PATH="${MONO_HOME}/bin":${PATH}
        # This is not pretty but some of the mono executable seem to hard code
        # dependencies path to this folder and I couldn't find an envvar to
        # overwrite that :(
        if [ ! -e "/Library/Frameworks/Mono.framework/Versions/6.12.0" ]; then
          sudo mkdir -p "/Library/Frameworks/Mono.framework/Versions"
          sudo ln -s -f "$MONO_HOME" "/Library/Frameworks/Mono.framework/Versions/6.12.0"
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# path
export PATH="${HAM_TOOLSET_DIR}":${PATH}

VER="--- dotnet_mono -------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`mono --version | grep version`"
    if [ $? != 0 ]; then
      echo "E/Can't get version."
      return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
