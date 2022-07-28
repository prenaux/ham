#!/bin/bash

toolset_import xslt_tools
if [ $? != 0 ]; then return 1; fi

if [ -z $HAM_MSVC_ARCH ]; then
    export HAM_MSVC_ARCH=x64
fi

TAG=msvc_19_${HAM_MSVC_ARCH}
echo I/Setting up $TAG

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

toolset_check_and_dl_ver msvc_19_x64 nt-x86 19b || return 1

export MSVCDIR="${HAM_TOOLSET_DIR}/nt-x86/2019/BuildTools/VC/Tools/MSVC/14.29.30133"
export MSVCDIR_BIN="${MSVCDIR}/bin/Host${HAM_MSVC_ARCH}/${HAM_MSVC_ARCH}"
export MSVC_VER=19
if [ ! -e "${MSVCDIR_BIN}/cl.exe" ]; then
  echo "E/$HAM_TOOLSET_NAME: Couldn't find cl.exe"
  return 1
fi

########################################################################
##  Find devenv.exe to setup RUN_DEBUGGER
########################################################################
export MSVC_IDE_DIR="`unxpath "$PROGRAMFILES\\Microsoft Visual Studio 12.0\\Common7\\IDE"`"
export RUN_DEBUGGER="${MSVC_IDE_DIR}/devenv.exe"
if [ ! -f "$RUN_DEBUGGER" ]; then
    export MSVC_IDE_DIR="`unxpath "$PROGRAMFILES\\Microsoft Visual Studio 11.0\\Common7\\IDE"`"
    export RUN_DEBUGGER="${MSVC_IDE_DIR}/devenv.exe"
    if [ ! -f "$RUN_DEBUGGER" ]; then
        export MSVC_IDE_DIR="`unxpath "$PROGRAMFILES\\Microsoft Visual Studio 10.0\\Common7\\IDE"`"
        export RUN_DEBUGGER="${MSVC_IDE_DIR}/devenv.exe"
        if [ ! -f "$RUN_DEBUGGER" ]; then
	          echo "E/Can't find debugger 'devenv.exe' in $RUN_DEBUGGER"
        fi
    fi
fi
if [ -f "$RUN_DEBUGGER" ]; then
    echo "I/Found VC++ debugger in '$RUN_DEBUGGER'"
fi
export RUN_DEBUGGER_PARAMS=-debugexe

########################################################################
##  Find gacutil.exe to setup WINSDKDIR
########################################################################
export WINSDKVER="10.0.18362.0"

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
##  Find Msbuild.exe and setup MSBUILD_EXE
########################################################################
export MSBUILD_DIR="${HAM_TOOLSET_DIR}/nt-x86/2019/BuildTools/MSBuild"
export MSBUILD_EXE="${MSBUILD_DIR}/Current/Bin/MSBuild.exe"
if [ ! -e "$MSBUILD_EXE" ]; then
	echo "E/Can't find '$MSBUILD_EXE'"
	return 1
fi
echo "I/Found MSBuild at '$MSBUILD_EXE'"

########################################################################
##  Setup the C++ environment
########################################################################
export HAM_CL="\"${MSVCDIR_BIN}/cl.exe\""
export HAM_LINK="\"${MSVCDIR_BIN}/link.exe\""
export PATH="${WINSDKDIR_BIN}":"${MSVCDIR_BIN}":"${MSVC_IDE_DIR}":${PATH}
export INCLUDE="`nativedir \"${WINSDKDIR_INCLUDE}/um\"`;`nativedir \"${WINSDKDIR_INCLUDE}/ucrt\"`;`nativedir \"${WINSDKDIR_INCLUDE}/shared\"`;`nativedir \"${MSVCDIR}/include\"`"
export LIB="`nativedir \"${WINSDKDIR_LIBS}/um/${HAM_MSVC_ARCH}\"`;`nativedir \"${WINSDKDIR_LIBS}/ucrt/${HAM_MSVC_ARCH}\"`;`nativedir \"${MSVCDIR}/lib/${HAM_MSVC_ARCH}\"`"

VER="--- Microsoft Visual C++ 19 ${HAM_MSVC_ARCH} -----------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`cl 2>&1 >/dev/null | grep Optimizing`"
    if [ $? != 0 ]; then
        echo "E/Can't get version."
        return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
