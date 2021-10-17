#!/bin/bash
. ham-toolset-import.sh xslt_tools
. ham-toolset-import.sh java_jdk
export BUILD_JNI=1

VER="--- build_jni ------------------------"
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
