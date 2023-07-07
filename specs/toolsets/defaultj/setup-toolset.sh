#!/bin/bash
toolset_import_once repos || return 1
toolset_import_once build_jni || return 1

TARGET_BIN_LOA=$(toolset_get_target_bin_loa)
case "${TARGET_BIN_LOA}" in
    nt-x86)
        toolset_import_once msvc_19_x86 || return 1
        ;;
    nt-x64)
        toolset_import_once msvc_19_x64 || return 1
        ;;
    osx-arm64)
        toolset_import_once macos_arm64 || return 1
        ;;
    osx-x64)
        toolset_import_once macos_x64 || return 1
        ;;
    lin-x64)
        toolset_import_once linux_x64 || return 1
        ;;
    *)
        echo "E/Toolset: Unsupported JNI TARGET_BIN_LOA '${TARGET_BIN_LOA}'"
        return 1
        ;;
esac
