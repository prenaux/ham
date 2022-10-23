#!/bin/bash
toolset_import_once xslt_tools || return 1

if [ -z $HAM_MSVC_ARCH ]; then
    export HAM_MSVC_ARCH=x64
fi

TAG=msvc_15_${HAM_MSVC_ARCH}
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
export HAM_TOOLSET_NAME=msvc_15_${HAM_MSVC_ARCH}
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/msvc_15_x64"
export HAM_CPP_TOOLSET=$HAM_TOOLSET
export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

export MSVCDIR="${HAM_TOOLSET_DIR}/nt-x86"
export VSINSTALLDIR="${HAM_TOOLSET_DIR}/nt-x86"
export MSVC_VER=15
if [ ! -e "$MSVCDIR/bin/cl.exe" ]; then
    toolset_dl msvc_15_x64 msvc_15_x64_nt-x86
    if [ ! -e "$MSVCDIR/bin/cl.exe" ]; then
        echo "E/nt-x86 folder doesn't exist in the toolset"
        return 1
    fi
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
export WINSDKDIR="${HAM_TOOLSET_DIR}/nt-x86/winsdk/10/"
if [ ! -e "$WINSDKDIR/bin/${HAM_MSVC_ARCH}/fxc.exe" ]; then
	echo "E/Can't find '$WINSDKDIR/bin/${HAM_MSVC_ARCH}/fxc.exe' in WinSDK for $TAG"
	return 1
fi
echo "I/Found WindowsSDK in '$WINSDKDIR'"
export WindowsSdkDir=$WINSDKDIR

export WINSDKDIR_LIBS="${WINSDKDIR}/Lib/10.0.10240.0"
if [ ! -e "$WINSDKDIR_LIBS/um/${HAM_MSVC_ARCH}/d3d12.lib" ]; then
	echo "E/Can't find d3d12.lib in '$WINSDKDIR_LIBS/d3d12.lib' for $TAG"
	return 1
fi

export WINSDKDIR_INCLUDE="${WINSDKDIR}/include/10.0.10240.0"
if [ ! -e "$WINSDKDIR_INCLUDE/um/windows.h" ]; then
	echo "E/Can't find windows.h in '$WINSDKDIR_INCLUDE/um/windows.h' for $TAG"
	return 1
fi

########################################################################
##  Setup the C++ environment
########################################################################
export INCLUDE="`nativedir \"${WINSDKDIR_INCLUDE}/um\"`;`nativedir \"${WINSDKDIR_INCLUDE}/ucrt\"`;`nativedir \"${WINSDKDIR_INCLUDE}/shared\"`;`nativedir \"${MSVCDIR}/include\"`;`nativedir \"${MSVCDIR}/atlmfc/include/\"`"

case "$HAM_MSVC_ARCH" in
    x86)
        export HAM_C99_FLAGS="-m32"
        export HAM_CL="\"$MSVCDIR/bin/cl.exe\""
        export HAM_LINK="\"$MSVCDIR/bin/link.exe\""
        export PATH="${WINSDKDIR}/bin/${HAM_MSVC_ARCH}":"${MSVCDIR}/bin":"${MSVCDIR}/Clang 3.7/bin/x86":"${MSVC_IDE_DIR}":${PATH}
        export LIB="`nativedir \"${WINSDKDIR_LIBS}/um/${HAM_MSVC_ARCH}\"`;`nativedir \"${WINSDKDIR_LIBS}/ucrt/${HAM_MSVC_ARCH}\"`;`nativedir \"${MSVCDIR}/lib/\"`;`nativedir \"${MSVCDIR}/atlmfc/lib/\"`"
    ;;
    x64)
        export HAM_C99_FLAGS="-m64"
        export HAM_CL="\"$MSVCDIR/bin/x86_amd64/cl.exe\""
        export HAM_LINK="\"$MSVCDIR/bin/x86_amd64/link.exe\""
        export PATH="${WINSDKDIR}/bin/${HAM_MSVC_ARCH}":"${MSVCDIR}/bin/x86_amd64":"${MSVCDIR}/Clang 3.7/bin/x86":"${MSVCDIR}/bin":"${MSVC_IDE_DIR}":${PATH}
        export LIB="`nativedir \"${WINSDKDIR_LIBS}/um/${HAM_MSVC_ARCH}\"`;`nativedir \"${WINSDKDIR_LIBS}/ucrt/${HAM_MSVC_ARCH}\"`;`nativedir \"${MSVCDIR}/lib/amd64\"`;`nativedir \"${MSVCDIR}/atlmfc/lib/amd64\"`"
    ;;
esac

export HAM_CROSS=
export HAM_C99=clang

VER="--- Microsoft Visual C++ 15 ${HAM_MSVC_ARCH} -----------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`cl 2>&1 >/dev/null | grep Optimizing`
`clang --version | grep CodeGen`"
    if [ $? != 0 ]; then
        echo "E/Can't get version."
        return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
