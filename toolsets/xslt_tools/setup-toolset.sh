#!/bin/bash

toolset_import java_jdk16
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=XSLT_TOOLS
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=xslt_tools
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/xslt_tools

# path setup
case $HAM_OS in
    NT*)
        export XSLT_TOOLS_BIN_DIR=${HAM_TOOLSET_DIR}/nt-x86/
        export PATH=${XSLT_TOOLS_BIN_DIR}:${PATH}
        if [ ! -e "$XSLT_TOOLS_BIN_DIR" ]; then
            toolset_dl xslt_tools xslt_tools_1_nt-x86
            if [ ! -e "$XSLT_TOOLS_BIN_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# jars
export XSLT_TOOLS_JARS_DIR=${HAM_TOOLSET_DIR}/jars/
if [ ! -e "$XSLT_TOOLS_JARS_DIR" ]; then
    toolset_dl xslt_tools xslt_tools_1_jars
    if [ ! -e "$XSLT_TOOLS_JARS_DIR" ]; then
        echo "E/jars folder doesn't exist in the toolset"
        return 1
    fi
fi

# path
export PATH=${HAM_TOOLSET_DIR}:${PATH}

# version
VER="--- xslt_tools_1 ----------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
--- xsltproc ---
`xsltproc --version | grep xsltproc`
--- saxon ------
`xslt_tools-saxon 2>&1 | grep Saxon`"
    if [ $? != 0 ]; then
        echo "E/Can't get version."
        return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
