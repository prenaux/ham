#!/bin/bash
toolset_import_once xslt_tools || return 1

TAG=msvc_10_x86
echo I/Setting up $TAG

########################################################################
##  Toolset
########################################################################
case $HAM_OS in
    NT*)
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

export HAM_TOOLSET=VISUALC
export HAM_TOOLSET_NAME=msvc_10_x86
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}"
export HAM_CPP_TOOLSET=$HAM_TOOLSET
export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

export MSVCDIR="${HAM_TOOLSET_DIR}/nt-x86"
export VSINSTALLDIR="${HAM_TOOLSET_DIR}/nt-x86"
export MSVC_VER=10
if [ ! -e "$MSVCDIR/bin/cl.exe" ]; then
    toolset_dl msvc_10_x86 msvc_10_x86_nt-x86
    if [ ! -e "$MSVCDIR/bin/cl.exe" ]; then
        echo "E/nt-x86 folder doesn't exist in the toolset"
        return 1
    fi
fi

########################################################################
##  Find cl.exe to setup MSVCDIR
########################################################################
if [ ! -e "$MSVCDIR/bin/cl.exe" ]; then
	echo "E/Can't find cl.exe for $TAG"
	return 1
fi
echo "I/Found VC++ in '$MSVCDIR'"

########################################################################
##  Find devenv.exe to setup RUN_DEBUGGER
########################################################################
export MSVC_IDE_DIR=$(toolset_find_msvc_ide_dir)
export RUN_DEBUGGER=$(toolset_find_msvc_devenv)
export RUN_DEBUGGER_PARAMS=-debugexe

########################################################################
##  Find gacutil.exe to setup WINSDKDIR
########################################################################
export WINSDKDIR="${HAM_TOOLSET_DIR}/nt-x86/winsdk"
if [ ! -e "$WINSDKDIR/bin/gacutil.exe" ]; then
	echo "E/Can't find gacutil.exe for WinSDK"
	return 1
fi
echo "I/Found WindowsSDK in '$WINSDKDIR'"
export WindowsSdkDir=$WINSDKDIR

########################################################################
##  Setup the C++ environment
########################################################################
export PATH="${WINSDKDIR}/bin":"${MSVCDIR}/bin":"${MSVC_IDE_DIR}":${PATH}
export INCLUDE="`nativedir \"${WINSDKDIR}/include\"`;`nativedir \"${MSVCDIR}/include\"`"
export LIB="`nativedir \"${WINSDKDIR}/lib\"`;`nativedir \"${MSVCDIR}/lib\"`"

export HAM_CL="\"$MSVCDIR/bin/cl.exe\""
export HAM_LINK="\"$MSVCDIR/bin/link.exe\""
export HAM_CROSS=

VER="--- Microsoft Visual C++ 10 x86 -----------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`cl 2>&1 >/dev/null`"
    if [ $? != 0 ]; then
        echo "E/Can't get version."
        return 1
    fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
