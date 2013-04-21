# Imports
toolset_import xslt_tools
if [ $? != 0 ]; then return 1; fi
toolset_import msvc_11_arm
if [ $? != 0 ]; then return 1; fi

# Toolset
export LIBPATH="`nativedir \"${WINSDKDIR}/References/CommonConfiguration/Neutral\"`;`nativedir \"${MSVCDIR}/vcpackages\"`"
export HAM_TOOLSET=VISUALC
export HAM_TOOLSET_VER=11
export HAM_TOOLSET_NAME=winrt_arm
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}
