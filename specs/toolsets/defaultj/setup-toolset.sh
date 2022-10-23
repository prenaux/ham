#!/bin/bash
toolset_import_once repos || return 1
toolset_import_once build_jni || return 1

# path setup
case $HAM_OS in
    NT*)
        toolset_import default || return 1
        ;;
    OSX*)
        toolset_import default || return 1
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
