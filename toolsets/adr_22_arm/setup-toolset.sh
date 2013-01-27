#!/bin/bash

# import dependencies
toolset_import build_jni
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET_IS_SETUP_ADR_22_ARM=1
export HAM_TOOLSET=ANDROID
export HAM_TOOLSET_VER=2
export HAM_TOOLSET_NAME=adr_22_arm
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/adr_22_arm

# adr_22_arm setup
export ADR_22_ARM_ROOT=${HAM_TOOLSET_DIR}/nt-x86

# dl if missing
if [ ! -e $ADR_22_ARM_ROOT  ]; then
    toolset_dl adr_22_arm adr_22_arm
    if [ ! -e $ADR_22_ARM_ROOT ]; then
        echo "adr_22_arm folder doesn't exist in the toolset"
        return 1
    fi
fi

# set JVM mem (need for big code base to build and for the JVM to not sometime
# fail to instantiate)
export _JAVA_OPTIONS="-Xms256m -Xmx768m"

export ADR_PLATFORM=7
export ADR_CPU_PROFILE=ARMv6
export ADR_DIR_BASE=${ADR_22_ARM_ROOT}
export ADR_DIR_NDK=${ADR_22_ARM_ROOT}/ndk_r4
export ADR_DIR_NDK_USR=${ADR_DIR_NDK}/build/platforms/android-5/arch-arm/usr

export CYGWIN=nodosfilewarning # disable awesome cygwin warning...

export GCC_VER=4.4.0
export GCC_BASE=arm-eabi
export GCC_EXE_BASE=${GCC_BASE}-
export GCC_DIR=${ADR_DIR_NDK}/build/prebuilt/windows/${GCC_EXE_BASE}${GCC_VER}

export PATH=${ADR_DIR_BASE}/scripts:${GCC_DIR}/bin:${ADR_DIR_BASE}/sdk/tools:${ADR_DIR_BASE}/sdk/platform-tools:${PATH}

VER="--- adr_22_arm ------------------------
`${GCC_EXE_BASE}gcc --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
