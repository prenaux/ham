#!/bin/bash
. ham-toolset-import.sh repos
case $HAM_OS in
    NT*)
        . ham-toolset-import.sh msvc_15_x64
        ;;
    OSX*)
        if [ "$HAM_BIN_LOA" == "osx-arm64" ]; then
            . ham-toolset-import.sh macos_arm64
        else
            . ham-toolset-import.sh macos_x64
        fi
        ;;
    LINUX)
        . ham-toolset-import.sh linux_x64
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
