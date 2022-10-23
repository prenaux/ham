#!/bin/bash
toolset_import_once xslt_tools || return 1
toolset_import_once java_jdk || return 1
export BUILD_JNI=1

VER="--- build_jni ------------------------"
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
