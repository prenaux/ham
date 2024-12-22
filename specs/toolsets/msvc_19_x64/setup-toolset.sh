#!/bin/bash
toolset_import_once xslt_tools || return 1

if [ -z "$HAM_MSVC_ARCH" ]; then
  export HAM_MSVC_ARCH=x64
fi

TAG=msvc_19_${HAM_MSVC_ARCH}
echo "I/Setting up $TAG"

########################################################################
##  Toolset
########################################################################
case $HAM_OS in
  NT*)
    export BUILD_BIN_LOA=nt-${HAM_MSVC_ARCH}
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

export HAM_TOOLSET=VISUALC
export HAM_TOOLSET_NAME=msvc_19_${HAM_MSVC_ARCH}
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/msvc_19_x64"
export HAM_CPP_TOOLSET=$HAM_TOOLSET
export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

toolset_check_and_dl_ver msvc_19_x64 nt-x86 v24_12 || return 1

export VCTOOLSVERSION="14.42.34433"
export WINSDKVER="10.0.26100.0"

export MSVCDIR="${HAM_TOOLSET_DIR}/nt-x86/VC/Tools/MSVC/${VCTOOLSVERSION}"
export MSVCDIR_BIN="${MSVCDIR}/bin/Host${HAM_MSVC_ARCH}/${HAM_MSVC_ARCH}"
export MSVC_VER=19
if [ ! -e "${MSVCDIR_BIN}/cl.exe" ]; then
  echo "E/$HAM_TOOLSET_NAME: Couldn't find cl.exe in MSVCDIR_BIN '$MSVCDIR_BIN'."
  return 1
fi

########################################################################
##  Find devenv.exe to setup RUN_DEBUGGER
########################################################################

# NOTE: This is quite slow and should be used only once during setup.
toolset_find_msvc_ide_dir() {
  # We assume that all combinations of visual studio versions and editions
  # can be located in both the x86 and x64 program files directories...
  local vs_pf_dirs=(
    "$PROGRAMW6432\\Microsoft Visual Studio"
    "$PROGRAMFILES\\Microsoft Visual Studio")

  # Version arrays for the different types of visual studio licenses
  # NOTE: the following arrays must be same length
  local vs_versions_typed=(10 2020)
  local vs_versions_typed_prefix=(" " "\\")
  local vs_versions_typed_suffix=(".0" "\\Community")

  # Count of the supported versions / years
  # eg. 2020 all the way to 2025
  # eg. 10.0 all the way to 15.0
  # NOTE: As of early 2023 community version is at 2022 and Visual Studio at 14.0
  local vs_versions_supported_count=6

  # loop through dirs, versions to find the IDE dir
  for vs_pf_dir in "${vs_pf_dirs[@]}"; do
    for i in "${!vs_versions_typed[@]}"; do
      local prefix_=${vs_versions_typed_prefix[$i]}
      local suffix_=${vs_versions_typed_suffix[$i]}
      local version_=${vs_versions_typed[$i]}
      for ((ry = vs_versions_supported_count; ry >= 1; ry--)); do
        local out_version_=$((version_ + ry - 1))
        local out_dir_="${vs_pf_dir}${prefix_}${out_version_}${suffix_}\\Common7\\IDE"
        # echo "I/ Checking for IDE dir: ${out_dir_}"
        # We check for the devenv.exe file, because the directory may exist
        # but be empty.
        if [ -d "$out_dir_" ] && [ -f "$out_dir_\\devenv.exe" ]; then
          echo "${out_dir_}"
          return 0
        fi
      done
    done
  done

  ide_dir="c:/Program Files (x86)/Microsoft Visual Studio 12.0/Common7/IDE/"
  if [ -d "$ide_dir" ] && [ -f "$ide_dir/devenv.exe" ]; then
    echo "${ide_dir}"
    return 0
  fi

  echo "__cant_find_msvc_ide_dir__"
  return 1
}

MSVC_IDE_DIR=$(toolset_find_msvc_ide_dir)
export MSVC_IDE_DIR
echo "I/MSVC_IDE_DIR '$MSVC_IDE_DIR'"
RUN_DEBUGGER="$MSVC_IDE_DIR/devenv.exe"
export RUN_DEBUGGER
export RUN_DEBUGGER_PARAMS=-debugexe

########################################################################
##  Find gacutil.exe to setup WINSDKDIR
########################################################################
export WINSDKDIR="${HAM_TOOLSET_DIR}/nt-x86/winsdk/10"

export WINSDKDIR_BIN="${WINSDKDIR}/bin/${WINSDKVER}/${HAM_MSVC_ARCH}/"
if [ ! -e "$WINSDKDIR_BIN/fxc.exe" ]; then
  echo "E/Can't find '$WINSDKDIR/bin/${HAM_MSVC_ARCH}/fxc.exe' in WinSDK for $TAG"
  return 1
fi
echo "I/Found WindowsSDK in '$WINSDKDIR'"
export WindowsSdkDir=$WINSDKDIR

export WINSDKDIR_LIBS="${WINSDKDIR}/Lib/${WINSDKVER}"
if [ ! -e "$WINSDKDIR_LIBS/um/${HAM_MSVC_ARCH}/d3d12.lib" ]; then
  echo "E/Can't find d3d12.lib in '$WINSDKDIR_LIBS/d3d12.lib' for $TAG"
  return 1
fi

export WINSDKDIR_INCLUDE="${WINSDKDIR}/include/${WINSDKVER}"
if [ ! -e "$WINSDKDIR_INCLUDE/um/windows.h" ]; then
  echo "E/Can't find windows.h in '$WINSDKDIR_INCLUDE/um/windows.h' for $TAG"
  return 1
fi

########################################################################
##  Setup the C++ environment
########################################################################
export HAM_CL="\"${MSVCDIR_BIN}/cl.exe\""
export HAM_LINK="\"${MSVCDIR_BIN}/link.exe\""
pathenv_add "${MSVCDIR_BIN}"
# Put winsdk's bin folder after so that it doesnt shadow utility scripts. Note
# that you can still call executables explicitly by specifying the .exe
# extension which is never used by any script.
pathenv_add "${WINSDKDIR_BIN}" after
pathenv_add "${MSVC_IDE_DIR}"

INCLUDE="$(nativedir "${WINSDKDIR_INCLUDE}/um");$(nativedir "${WINSDKDIR_INCLUDE}/ucrt");$(nativedir "${WINSDKDIR_INCLUDE}/shared");$(nativedir "${MSVCDIR}/include")"
export INCLUDE
LIB="$(nativedir "${WINSDKDIR_LIBS}/um/${HAM_MSVC_ARCH}");$(nativedir "${WINSDKDIR_LIBS}/ucrt/${HAM_MSVC_ARCH}");$(nativedir "${MSVCDIR}/lib/${HAM_MSVC_ARCH}")"
export LIB

VER="--- Microsoft Visual C++ 19 ${HAM_MSVC_ARCH} -----------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(cl 2>&1 >/dev/null | grep Optimizing)"; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
