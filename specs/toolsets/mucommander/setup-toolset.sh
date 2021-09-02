#!/bin/bash

toolset_import java_jdk18
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=MUCOMMANDER
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=mucommander
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/mucommander"

# jars
export MUCOMMANDER_JARS_DIR="${HAM_TOOLSET_DIR}/jars"
export MUCOMMANDER_JARS_MAIN_JAR="$MUCOMMANDER_JARS_DIR/mucommander-0.9.8.jar"
toolset_check_and_dl_ver mucommander jars v1 || return 1

# version
VER="--- mucommander ----------------------"
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
