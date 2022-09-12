#!/bin/bash

# import dependencies
toolset_import xslt_tools
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=QCC
export HAM_TOOLSET_NAME=qcc_660
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/qnx_660"
export HAM_CPP_TOOLSET=$HAM_TOOLSET
export HAM_CPP_TOOLSET_NAME=$HAM_TOOLSET_NAME

# path setup
case $HAM_OS in
    LINUX)
        QNX_ENV="${HAM_TOOLSET_DIR}/lin-x64/qnx660-env.sh"
        if [ ! -e "${QNX_ENV}" ]; then
            toolset_dl qnx_660 qnx_660_lin-x64
            if [ ! -e "${QNX_ENV}" ]; then
                echo "E/lin-x64 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        export QNX_HOME="${HAM_TOOLSET_DIR}/lin-x64"
        source "${QNX_ENV}"
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- qnx_660 ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`qcc -V`"
    if [ $? != 0 ]; then
      echo "E/Can't get version."
      return 1
    fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
