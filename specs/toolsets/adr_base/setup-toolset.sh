#!/bin/bash
toolset_import_once xslt_tools || return 1
toolset_import_once build_jni || return 1
toolset_import_once ant || return 1

# Supported Android Platforms
# 42: Android 4.2, 4.2.2    17    JELLY_BEAN_MR1
# 22: Android 2.2.x          8    FROYO
if [ -z "$ADR_VERSION" ]; then
  echo "E/Android toolset: version not defined !"
  return 1
fi

export ADR_SDK_PLATFORM=android-$ADR_API
export ADR_NDK_VERSION=23.1.7779620

# toolset
export HAM_TOOLSET=ANDROID
export HAM_TOOLSET_NAME=adr_${ADR_VERSION}_${ADR_CPU_TYPE}
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/adr_base"
export HAM_CPP_TOOLSET=$HAM_TOOLSET
export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

# adr setup
case $HAM_OS in
  NT)
    export ADR_NDK_PREBUILT=windows
    export ADR_DIR_NDK="${HAM_TOOLSET_DIR}/nt-x86/ndk_r12b"
    export ADR_DIR_SDK="${HAM_TOOLSET_DIR}/nt-x86/sdk"
    export GCC_VER=4.9
    # dl if missing
    if [ ! -e "$ADR_DIR_SDK" ] || [ ! -e "$ADR_DIR_NDK" ]; then
      toolset_dl adr_base adr_base_nt-x86
      if [ ! -e "$ADR_DIR_SDK" ] || [ ! -e "$ADR_DIR_NDK" ]; then
        echo "adr_base nt-x86 folder doesn't exist in the toolset"
        return 1
      fi
    fi
    ;;
  OSX)
    SYSTEM_NAME=$(uname)-$(uname -m)
    export SYSTEM_NAME
    export ADR_NDK_PREBUILT=${SYSTEM_NAME,,}
    export ADR_DIR_SDK="/usr/local/share/android-sdk"
    export ADR_DIR_NDK="${ADR_DIR_SDK}/ndk/${ADR_NDK_VERSION}"
    chmod +x "${HAM_TOOLSET_DIR}/adr-"*

    # Test the SDK
    if [ ! -d "$ADR_DIR_SDK" ] || [ ! -d "${ADR_DIR_SDK}/platforms/${ADR_SDK_PLATFORM}" ]; then
      echo "I/Can't find android sdk, installing with brew..."
      ham-brew install --cask android-sdk android-platform-tools
      echo "I/Making sure the Android SDK isn't quarantined..."
      sudo xattr -r -d com.apple.quarantine /usr/local/Caskroom/android-sdk/
      sudo xattr -r -d com.apple.quarantine /usr/local/Caskroom/android-platform-tools/
      echo "I/Downloading platform ${ADR_SDK_PLATFORM}..."
      touch ~/.android/repositories.cfg
      sdkmanager --update
      sdkmanager "platform-tools" "platforms;${ADR_SDK_PLATFORM}"
      sdkmanager --install "ndk;${ADR_NDK_VERSION}" --channel=3
      echo "I/Android SDK and requirements should now be installed."
    fi
    if [ ! -d "$ADR_DIR_SDK" ] || [ ! -d "${ADR_DIR_SDK}/platforms/${ADR_SDK_PLATFORM}" ]; then
      echo "adr_base osx can't install the android sdk & ndk & platform"
      return 1
    fi
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

case $ADR_CPU_TYPE in
  arm64)
    export GCC_BASE=aarch64-linux-android
    export GCC_BASE_DIR=${GCC_BASE}
    export ADR_CPU_PROFILE=ARM64v8A
    export ADR_CPU_ABI=arm64-v8a
    ;;
  arm)
    export GCC_BASE=arm-linux-androideabi
    export GCC_BASE_DIR=${GCC_BASE}
    export ADR_CPU_PROFILE=ARMv7A
    export ADR_CPU_ABI=armeabi-v7a
    ;;
  x86)
    export GCC_BASE=i686-linux-android
    export GCC_BASE_DIR=x86
    export ADR_CPU_PROFILE=X86
    export ADR_CPU_ABI=x86
    ;;
  *)
    echo "E/Android toolset: Unsupported CPU: '${ADR_CPU_TYPE}' !"
    return 1
    ;;
esac

export ADR_GCC_OPT=-O2

export ADR_SDK_PLATFORM_DIR="${ADR_DIR_SDK}/platforms/${ADR_SDK_PLATFORM}"
if [ ! -e "$ADR_SDK_PLATFORM_DIR" ]; then
  echo "E/Android toolset: can't find SDK platform:" "${ADR_SDK_PLATFORM_DIR}"
  return 1
fi

pathenv_add "${HAM_TOOLSET_DIR}"
pathenv_add "${ADR_DIR_BASE}/scripts"
pathenv_add "${ADR_DIR_SDK}/tools"
pathenv_add "${ADR_DIR_SDK}/platform-tools"

export CYGWIN=nodosfilewarning # disable awesome cygwin warning...

export ADR_LLVM_NAME=llvm
export ADR_LLVM_TOOLCHAIN_ROOT="${ADR_DIR_NDK}/toolchains/${ADR_LLVM_NAME}"
export ADR_LLVM_TOOLCHAIN_PREBUILT_ROOT="${ADR_LLVM_TOOLCHAIN_ROOT}/prebuilt/$ADR_NDK_PREBUILT"
export ADR_LLVM_TOOLCHAIN_PREFIX="${ADR_LLVM_TOOLCHAIN_PREBUILT_ROOT}/bin/"
export ADR_DIR_NDK_USR="${ADR_LLVM_TOOLCHAIN_PREBUILT_ROOT}/sysroot/usr"
export ADR_DIR_NDK_PLATFORM="${ADR_LLVM_TOOLCHAIN_PREBUILT_ROOT}/sysroot/usr/lib/arm-linux-androideabi/21"
pathenv_add "${ADR_LLVM_TOOLCHAIN_PREFIX}"

export ANDROID_HOME=${ADR_DIR_SDK}

VER="--- adr-clang ------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(clang --version)"; then
    echo "E/Can't get clang version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
