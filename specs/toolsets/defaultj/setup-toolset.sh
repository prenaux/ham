#!/bin/bash

# path setup
case $HAM_OS in
    NT*)
        . ham-toolset-import.sh build_jni
        . ham-toolset-import.sh default
        ;;
    OSX*)
        . ham-toolset-import.sh build_jni
        . ham-toolset-import.sh default
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
