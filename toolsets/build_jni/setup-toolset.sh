#!/bin/bash
. ham-toolset-import.sh java_jdk16
export BUILD_JNI=1

VER="--- build_jni ------------------------"
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
