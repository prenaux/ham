#!/bin/bash

# toolset
export HAM_TOOLSET=FFMPEG
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=ffmpeg
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/ffmpeg"

# path setup
case $HAM_OS in
    NT*)
        toolset_check_and_dl_ver ffmpeg nt-x86 v2 || return 1
        export FFMPEG_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH="${FFMPEG_DIR}":${PATH}
        ;;
    OSX*)
        toolset_check_and_dl_ver ffmpeg osx-x64 v3 || return 1
        export FFMPEG_DIR="${HAM_TOOLSET_DIR}/osx-x64/"
        export PATH="${FFMPEG_DIR}":${PATH}
        ;;
    LINUX)
        # ffmpeg is setup by ham-install-os-packages
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# path
export PATH="${HAM_TOOLSET_DIR}":${PATH}

VER="--- ffmpeg ------------------------
`ffmpeg 2>&1 | grep 'ffmpeg version'`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
