TAG=VC10-x86
echo I/Setting up $TAG

########################################################################
##  Find cl.exe to setup MSVCDIR
########################################################################
export MSVCDIR="`unxpath "$PROGRAMFILES\\Microsoft Visual Studio 10.0\\VC"`"
if [ ! -e "$MSVCDIR/bin/cl.exe" ]; then
	echo "E/Can't find cl.exe for $TAG"
	return 1
fi
echo "I/Found VC++ in '$MSVCDIR'"

########################################################################
##  Find devenv.exe to setup RUN_DEBUGGER
########################################################################
export RUN_DEBUGGER="${MSVCDIR}/../Common7/IDE/devenv.exe"
if [ ! -f "$RUN_DEBUGGER" ]; then
	echo "E/Can't find debugger 'devenv.exe' for $TAG"
else
    echo "I/Found VC++ debugger in '$RUN_DEBUGGER'"
fi
export RUN_DEBUGGER_PARAMS=-debugexe

########################################################################
##  Find gacutil.exe to setup WINSDKDIR
########################################################################
export WINSDKDIR="`unxpath "$PROGRAMFILES\\Microsoft SDKs\\Windows\\v7.0A"`"
if [ ! -e "$WINSDKDIR/bin/gacutil.exe" ]; then
	echo "E/Can't find gacutil.exe for WinSDK"
	return 1
fi
echo "I/Found WindowsSDK in '$WINSDKDIR'"

########################################################################
##  Setup the C++ environment
########################################################################
export HAM_TOOLSET=VISUALC
export HAM_TOOLSET_VER=10
export HAM_TOOLSET_NAME=VC10_X86

export PATH="${WINSDKDIR}/bin":"${MSVCDIR}/bin":"${MSVCDIR}/../Common7/IDE":${PATH}
export INCLUDE="`nativedir \"${WINSDKDIR}/include\"`;`nativedir \"${MSVCDIR}/include\"`"
export LIB="`nativedir \"${WINSDKDIR}/lib\"`;`nativedir \"${MSVCDIR}/lib\"`"

export HAM_CL="\"$MSVCDIR/bin/cl.exe\""
export HAM_LINK="\"$MSVCDIR/bin/link.exe\""
export HAM_CROSS=

return 0
