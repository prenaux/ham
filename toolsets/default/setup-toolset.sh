#!/bin/bash

# path setup
case $HAM_OS in
    NT*)
        . ham-toolset-import.sh xslt_tools
        MSVCDIR="`unxpath "$PROGRAMFILES\\Microsoft Visual Studio 11.0\\VC"`"
        if [ -e "$MSVCDIR/bin/cl.exe" ]; then
            echo "I/Default Toolset: Detected VS2012"
            . ham-toolset-import.sh msvc_11_x86
        else
            echo "I/Default Toolset: Using VC10 legacy package"
            . ham-toolset-import.sh msvc_10_x86
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
