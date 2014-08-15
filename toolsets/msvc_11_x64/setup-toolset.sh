TAG=msvc_11_x64
echo I/Setting up $TAG

case $HAM_OS in
    NT*)
        export BUILD_BIN_LOA=nt-x64
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

########################################################################
##  Find cl.exe to setup MSVCDIR
########################################################################
export MSVCDIR="`unxpath "$PROGRAMFILES\\Microsoft Visual Studio 12.0\\VC"`"
export MSVC_VER=12
if [ ! -e "$MSVCDIR/bin/x86_amd64/cl.exe" ]; then
    export MSVCDIR="`unxpath "$PROGRAMFILES\\Microsoft Visual Studio 11.0\\VC"`"
    export MSVC_VER=11
    if [ ! -e "$MSVCDIR/bin/x86_amd64/cl.exe" ]; then
	      echo "E/Can't find cl.exe for $TAG"
	      return 1
    fi
fi
echo "I/Found VC++ in '$MSVCDIR'"

########################################################################
##  Find devenv.exe to setup RUN_DEBUGGER
########################################################################
export MSVC_IDE_DIR="${MSVCDIR}/../Common7/IDE"
export RUN_DEBUGGER="${MSVC_IDE_DIR}/devenv.exe"
if [ ! -f "$RUN_DEBUGGER" ]; then
	echo "E/Can't find debugger 'devenv.exe' for $TAG"
else
    echo "I/Found VC++ debugger in '$RUN_DEBUGGER'"
fi
export RUN_DEBUGGER_PARAMS=-debugexe

########################################################################
##  Find gacutil.exe to setup WINSDKDIR
########################################################################
export WINSDKDIR="`unxpath "$PROGRAMFILES\\Windows Kits\\8.1"`"
export WINSDKDIR_LIBS="${WINSDKDIR}/Lib/winv6.3/um/x64"
if [ ! -e "$WINSDKDIR/bin/x64/fxc.exe" ]; then
    export WINSDKDIR="`unxpath "$PROGRAMFILES\\Windows Kits\\8.0"`"
    export WINSDKDIR_LIBS="${WINSDKDIR}/lib/win8/um/x64"
    if [ ! -e "$WINSDKDIR/bin/x64/fxc.exe" ]; then
	      echo "E/Can't find fxc.exe for WinSDK"
	      return 1
    fi
fi
echo "I/Found WindowsSDK in '$WINSDKDIR'"

########################################################################
##  Setup the C++ environment
########################################################################
export HAM_TOOLSET=VISUALC
export HAM_TOOLSET_VER=11
export HAM_TOOLSET_NAME=msvc_11_x64
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}

export PATH="${WINSDKDIR}/bin/x64":"${MSVCDIR}/bin/x86_amd64":"${MSVCDIR}/bin":"${MSVC_IDE_DIR}":${PATH}
export INCLUDE="`nativedir \"${WINSDKDIR}/include/um\"`;`nativedir \"${WINSDKDIR}/include/shared\"`;`nativedir \"${MSVCDIR}/include\"`;`nativedir \"${MSVCDIR}/atlmfc/include/\"`"
export LIB="`nativedir \"${WINSDKDIR_LIBS}\"`;`nativedir \"${MSVCDIR}/lib/amd64\"`;`nativedir \"${MSVCDIR}/atlmfc/lib/amd64\"`"

export HAM_CL="\"$MSVCDIR/bin/x86_amd64/cl.exe\""
export HAM_LINK="\"$MSVCDIR/bin/x86_amd64/link.exe\""
export HAM_CROSS=

VER="--- Microsoft Visual C++ 11 x64 -----------------"
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
