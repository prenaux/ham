#!/bin/bash

# toolset
export HAM_TOOLSET=FFMPEG
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=ffmpeg
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/ffmpeg"

# path setup
case $HAM_OS in
    NT*)
        export FFMPEG_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH="${FFMPEG_DIR}":${PATH}
        if [ ! -e "$FFMPEG_DIR/ffmpeg.exe" ]; then
            toolset_dl ffmpeg ffmpeg_nt-x86
            if [ ! -e "$FFMPEG_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- ffmpeg ------------------------
`ffmpeg 2>&1 | grep 'ffmpeg version'`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
