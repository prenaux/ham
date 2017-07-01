#!/bin/bash
case $HAM_OS in
    NT*)
        . ham-toolset-import.sh msvc_15_x64
        ;;
    OSX*)
        . ham-toolset-import.sh macos_x64
        ;;
    LINUX)
        . ham-toolset-import.sh gcc_470
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
