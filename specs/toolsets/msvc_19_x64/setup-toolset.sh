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

toolset_check_and_dl_ver msvc_19_x64 nt-x86 v24_13 || return 1

export VCTOOLSVERSION="14.42.34433"
export WINSDKVER="10.0.26100.0"

export MSVCDIR="${HAM_TOOLSET_DIR}/nt-x86/VC/Tools/MSVC/${VCTOOLSVERSION}"
export MSVCDIR_BIN="${MSVCDIR}/bin/Host${HAM_MSVC_ARCH}/${HAM_MSVC_ARCH}"
export MSVC_VER=19
if [ ! -e "${MSVCDIR_BIN}/cl.exe" ]; then
  echo "E/$HAM_TOOLSET_NAME: Couldn't find cl.exe in MSVCDIR_BIN '$MSVCDIR_BIN'."
  return 1
fi

pathenv_add "${HAM_TOOLSET_DIR}"

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
