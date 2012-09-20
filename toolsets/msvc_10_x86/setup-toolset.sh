TAG=VC10-x86
echo I/Setting up $TAG

case $HAM_OS in
    NT*)
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# Toolset
export HAM_TOOLSET_IS_SETUP_MSVC_10_X86=1
export HAM_TOOLSET=VISUALC
export HAM_TOOLSET_VER=10
export HAM_TOOLSET_NAME=msvc_10_x86
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}

export MSVCDIR="${HAM_TOOLSET_DIR}/nt-x86"
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
export MSVC_IDE_DIR="`unxpath "$PROGRAMFILES\\Microsoft Visual Studio 10.0\\Common7\\IDE"`"
export RUN_DEBUGGER="${MSVC_IDE_DIR}/devenv.exe"
if [ ! -f "$RUN_DEBUGGER" ]; then
	echo "E/Can't find debugger 'devenv.exe' in $RUN_DEBUGGER"
else
    echo "I/Found VC++ debugger in '$RUN_DEBUGGER'"
fi
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

########################################################################
##  Setup the C++ environment
########################################################################
export PATH="${WINSDKDIR}/bin":"${MSVCDIR}/bin":"${MSVC_IDE_DIR}":${PATH}
export INCLUDE="`nativedir \"${WINSDKDIR}/include\"`;`nativedir \"${MSVCDIR}/include\"`"
export LIB="`nativedir \"${WINSDKDIR}/lib\"`;`nativedir \"${MSVCDIR}/lib\"`"

export HAM_CL="\"$MSVCDIR/bin/cl.exe\""
export HAM_LINK="\"$MSVCDIR/bin/link.exe\""
export HAM_CROSS=

VER="--- Microsoft Visual C++ ------------------------
`cl 2>&1 >/dev/null`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
