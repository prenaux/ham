#!/bin/bash
export HAM_TOOLSET=vulkan_sdk
export HAM_TOOLSET_NAME=vulkan_sdk
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/vulkan_sdk"

case "$HAM_BIN_LOA" in
  osx-x64 | osx-arm64)
    ham-brew-install vulkan-headers "include/vulkan/vulkan.h"
    ham-brew-install glslang "bin/glslang"
    ham-brew-install spirv-tools "bin/spirv-opt"
    ham-brew-install molten-vk "bin/MoltenVKShaderConverter"
    ham-brew-install vulkan-tools "bin/vulkaninfo"
    ham-brew-install vulkan-profiles "share/vulkan/explicit_layer.d/VkLayer_khronos_profiles.json"
    ham-brew-install vulkan-validationlayers "share/vulkan/explicit_layer.d/VkLayer_khronos_validation.json"
    export VULKAN_SDK_INCDIR="$(ham-brew-installdir vulkan-headers include)"
    export VK_LAYER_PATH="$(ham-brew-installdir vulkan-validationlayers)/share/vulkan/explicit_layer.d"
    # MoltenVK specific
    export VULKAN_SDK_MOLTENVK_INCDIR="$(ham-brew-installdir molten-vk include)"
    export VULKAN_SDK_MOLTENVK_LIBDIR="$(ham-brew-installdir molten-vk lib)"
    export VK_ICD_FILENAMES="$(ham-brew-installdir molten-vk)/share/vulkan/icd.d/MoltenVK_icd.json"
    ;;
  lin-x64)
    # Note: This is only supported on Ubuntu derivatives
    toolset_check_and_dl_ver vulkan_sdk lin-x64 v1_3_296 || return 1
    VULKAN_SDK_SETUP_ENV="${HAM_TOOLSET_DIR}/lin-x64/setup-env.sh"
    source "$VULKAN_SDK_SETUP_ENV"
    if [ -z "$VULKAN_SDK" ]; then
      log_error "VULKAN_SDK not defined after running '$VULKAN_SDK_SETUP_ENV'."
      return 1
    fi
    if [ -z "$VK_ADD_LAYER_PATH" ]; then
      log_error "VK_ADD_LAYER_PATH not defined after running '$VULKAN_SDK_SETUP_ENV'."
      return 1
    fi
    export VULKAN_SDK_INCDIR="${VULKAN_SDK}/include"
    export VK_LAYER_PATH="${VULKAN_SDK}/share/vulkan/explicit_layer.d"
    ;;
  nt-x86|nt-x64)
    export VULKAN_SDK="$HAM_TOOLSET_DIR/nt-x64"
    export VULKAN_SDK_INCDIR="${VULKAN_SDK}/Include"
    # Ok... its in Bin on Windows, why not I guess, making it different on
    # every OS keeps things interesting.
    export VK_LAYER_PATH="${VULKAN_SDK}/Bin"
    toolset_check_and_dl_ver vulkan_sdk nt-x64 v1_3_296 || return 1
    if [ ! -f "$VULKAN_SDK_INCDIR/vulkan/vulkan.h" ]; then
      log_error "Vulkan SDK install looks like it succeeded but the required file hasnt been found !"
      return 1
    fi
    pathenv_add "${VULKAN_SDK}/Bin"
    ;;
  *)
    complain vulkan_sdk "Unsupported arch '$HAM_BIN_LOA'."
    return 1
    ;;
esac
pathenv_add "$HAM_TOOLSET_DIR"

VER="--- vulkan_sdk ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if [ ! -e "$VK_LAYER_PATH/VkLayer_khronos_validation.json" ]; then
    complain vulkan_sdk "Cant find VK_LAYER_PATH '$VK_LAYER_PATH/VkLayer_khronos_validation.json'." && return 1
  fi

  VER="$VER
VULKAN_SDK_INCDIR: $VULKAN_SDK_INCDIR
VK_LAYER_PATH: $VK_LAYER_PATH"

  if [ -n "$VULKAN_SDK_MOLTENVK_INCDIR" ]; then
    if [ ! -e "$VK_ICD_FILENAMES" ]; then
      complain vulkan_sdk "Cant find VK_ICD_FILENAMES '$VK_ICD_FILENAMES'." && return 1
    fi

    VER="$VER
VULKAN_SDK_MOLTENVK_INCDIR: $VULKAN_SDK_MOLTENVK_INCDIR
VULKAN_SDK_MOLTENVK_LIBDIR: $VULKAN_SDK_MOLTENVK_LIBDIR
VK_ICD_FILENAMES: $VK_ICD_FILENAMES"
  fi

  if ! VER="$VER
glslang: $(glslang --version | grep Glslang)"; then
    echo "E/Can't get version."
    return 1
  fi
  if ! VER="$VER
spirv-opt: $(spirv-opt --version 2>&1)"; then
    echo "E/Can't get version."
    return 1
  fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
