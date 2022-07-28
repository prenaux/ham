#!/bin/bash
toolset_import_once repos || return 1
case $HAM_OS in
    NT*)
        toolset_import_once msvc_19_x64 || return 1
        ;;
    OSX*)
        if [ "$HAM_BIN_LOA" == "osx-arm64" ]; then
            toolset_import_once macos_arm64 || return 1
        else
            toolset_import_once macos_x64 || return 1
        fi
        ;;
    LINUX)
        toolset_import_once linux_x64 || return 1
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
