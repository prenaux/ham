#!/bin/bash

# import dependencies
toolset_import build_jni
if [ $? != 0 ]; then return 1; fi
toolset_import ant
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=ANDROID
export HAM_TOOLSET_VER=2
export HAM_TOOLSET_NAME=adr_22_arm
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/adr_22_arm

# adr_22_arm setup
export ADR_22_ARM_ROOT=${HAM_TOOLSET_DIR}/nt-x86

# dl if missing
if [ ! -e $ADR_22_ARM_ROOT  ]; then
    toolset_dl adr_22_arm_nt-x86 adr_22_arm_nt-x86
    if [ ! -e $ADR_22_ARM_ROOT ]; then
        echo "adr_22_arm folder doesn't exist in the toolset"
        return 1
    fi
fi

export ADR_NDK_PREBUILT=windows
# export ADR_NDK_PREBUILT=windows-x86_64

export ADR_GCC_OPT=-O2
export ADR_CPU_PROFILE=ARMv7A
export ADR_CPU_ABI=armeabi-v7a
export ADR_NDK_PLATFORM=android-8
export ADR_SDK_PLATFORM=android-7
export ADR_NDK_VERSION=r8cx
export GCC_VER=4.7

# export ADR_GCC_OPT=-Os
# export ADR_CPU_PROFILE=ARMv6
# export ADR_CPU_ABI=armeabi
# export ADR_NDK_VERSION=r8e
# export ADR_NDK_PLATFORM=android-5
# export ADR_SDK_PLATFORM=android-7
# export GCC_VER=4.4.3

export ADR_DIR_BASE=${ADR_22_ARM_ROOT}
export ADR_DIR_NDK=${ADR_22_ARM_ROOT}/ndk_${ADR_NDK_VERSION}
export ADR_DIR_NDK_USR=${ADR_DIR_NDK}/platforms/$ADR_NDK_PLATFORM/arch-arm/usr

export ADR_SYSTEM_LINKLIBS="-L${ADR_DIR_NDK}/sources/crystax/libs/armeabi-v7a -lcrystax_static"

export CYGWIN=nodosfilewarning # disable awesome cygwin warning...

export GCC_BASE=arm-linux-androideabi
export GCC_EXE_BASE=${GCC_BASE}-
export GCC_DIR=${ADR_DIR_NDK}/toolchains/arm-linux-androideabi-${GCC_VER}/prebuilt/$ADR_NDK_PREBUILT
export ADR_LIBGCC_PATH=${GCC_DIR}/lib/gcc/arm-linux-androideabi/${GCC_VER}/libgcc.a

export PATH=${ADR_DIR_BASE}/scripts:${GCC_DIR}/bin:${ADR_DIR_BASE}/sdk/tools:${ADR_DIR_BASE}/sdk/platform-tools:${PATH}

VER="--- android ------------------------
cpu: $ADR_CPU_PROFILE, $ADR_CPU_ABI
sdk: $ADR_SDK_PLATFORM
ndk: $ADR_NDK_VERSION, $ADR_NDK_PLATFORM, $ADR_NDK_PREBUILT
--- adr_22_arm-gcc ------------------------
`${GCC_EXE_BASE}gcc --version`"
if [ $? != 0 ]; then
    echo "E/Can't get gcc version."
    return 1
fi

export ADR_LIBCPP_DIR_INCLUDE=$ADR_DIR_NDK/sources/cxx-stl/stlport/stlport
export ADR_LIBCPP_DIR_LIBS=$ADR_DIR_NDK/sources/cxx-stl/stlport/libs/$ADR_CPU_ABI
export ADR_LIBCPP_DEFINES="-DANDROID_STLPORT"
export ADR_LIBCPP_LINKER_LIB="-DANDROID_STLPORT -lstlport_static"

# export ADR_LIBCPP_DIR_INCLUDE=$ADR_DIR_NDK/sources/cxx-stl/gnu-libstdc++/$GCC_VER/include
# export ADR_LIBCPP_DIR_LIBS=$ADR_DIR_NDK/sources/cxx-stl/gnu-libstdc++/$GCC_VER/libs/$ADR_CPU_ABI
# export ADR_LIBCPP_DEFINES="-DANDROID_GNUSTL"
# export ADR_LIBCPP_LINKER_LIB="-lgnustl_static -lsupc++"

# export ADR_LLVM_VERSION=

export ADR_LLVM_VERSION=3.2
export ADR_LLVM_NAME=llvm-${ADR_LLVM_VERSION}
export ADR_LLVM_TOOLCHAIN_ROOT=${ADR_DIR_NDK}/toolchains/${ADR_LLVM_NAME}
export ADR_LLVM_TOOLCHAIN_PREBUILT_ROOT=${ADR_LLVM_TOOLCHAIN_ROOT}/prebuilt/$ADR_NDK_PREBUILT
export ADR_LLVM_TOOLCHAIN_PREFIX=${ADR_LLVM_TOOLCHAIN_PREBUILT_ROOT}/bin/
export PATH=${ADR_LLVM_TOOLCHAIN_PREFIX}:${PATH}

VER="$VER
--- adr_22_arm-clang ------------------
`clang --version`"
if [ $? != 0 ]; then
    echo "E/Can't get clang version."
    return 1
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
