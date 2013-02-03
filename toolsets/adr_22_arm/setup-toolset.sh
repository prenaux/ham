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

export ADR_LLVM_VERSION=

export ADR_PLATFORM=7
export ADR_DIR_BASE=${ADR_22_ARM_ROOT}
export ADR_DIR_NDK=${ADR_22_ARM_ROOT}/ndk_r8
export ADR_DIR_NDK_USR=${ADR_DIR_NDK}/platforms/android-5/arch-arm/usr

export CYGWIN=nodosfilewarning # disable awesome cygwin warning...

export GCC_VER=4.6
export GCC_BASE=arm-linux-androideabi
export GCC_EXE_BASE=${GCC_BASE}-
export GCC_DIR=${ADR_DIR_NDK}/toolchains/arm-linux-androideabi-4.6/prebuilt/windows
export ADR_LIBGCC_PATH=${GCC_DIR}/lib/gcc/arm-linux-androideabi/4.6/libgcc.a

export PATH=${ADR_DIR_BASE}/scripts:${GCC_DIR}/bin:${ADR_DIR_BASE}/sdk/tools:${ADR_DIR_BASE}/sdk/platform-tools:${PATH}

VER="--- adr_22_arm ------------------------
`${GCC_EXE_BASE}gcc --version`"
if [ $? != 0 ]; then
    echo "E/Can't get gcc version."
    return 1
fi

export ADR_CPU_PROFILE=ARMv6
# Note that the ARMv7 build with CLang has some issue:
# C++ c:/Users/Pierre/.ham/obj/nisdk-gcc-adr-armj-ra/niGraphics/GDRV_IM.o
# C:/Users/Pierre/AppData/Local/Temp/GDRV_IM-537552.s: Assembler messages:
# C:/Users/Pierre/AppData/Local/Temp/GDRV_IM-537552.s:209: Error: expected register list
# clang++: error: assembler command failed with exit code 1 (use -v to see invocation)
# export ADR_CPU_PROFILE=ARMv7A

export ADR_LLVM_VERSION=3.1
export ADR_LLVM_NAME=llvm-${ADR_LLVM_VERSION}
export ADR_LLVM_TOOLCHAIN_ROOT=${ADR_DIR_NDK}/toolchains/${ADR_LLVM_NAME}
export ADR_LLVM_TOOLCHAIN_PREBUILT_ROOT=${ADR_LLVM_TOOLCHAIN_ROOT}/prebuilt/windows
export ADR_LLVM_TOOLCHAIN_PREFIX=${ADR_LLVM_TOOLCHAIN_PREBUILT_ROOT}/bin/
export PATH=${ADR_LLVM_TOOLCHAIN_PREFIX}:${PATH}
VER="--- adr_22_arm-clang ------------------
`clang --version`"
if [ $? != 0 ]; then
    echo "E/Can't get clang version."
    return 1
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
