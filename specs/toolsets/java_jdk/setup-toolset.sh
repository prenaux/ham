#!/bin/bash
case $HAM_OS in
    NT*)
        . ham-toolset-import.sh java_jdk18
        ;;
    OSX)
        . ham-toolset-import.sh java_jdk_11
        ;;
    LINUX)
        . ham-toolset-import.sh java_jdk18
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
