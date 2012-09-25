#!/bin/bash

# path setup
case $HAM_OS in
    NT*)
        . ham-toolset-import.sh xslt_tools
        . ham-toolset-import.sh java_jdk16
        . ham-toolset-import.sh msvc_10_x86
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
