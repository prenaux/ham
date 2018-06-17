#!/bin/bash

# toolset
export HAM_TOOLSET=NODEJS
export HAM_TOOLSET_VER=0_12
export HAM_TOOLSET_NAME=nodejs
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/nodejs"

# path setup
case $HAM_OS in
    NT*)
        export NODEJS_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export NODEJS_GLOBAL_MODULES_DIR="${NODEJS_DIR}node_modules"
        export PATH=${HAM_TOOLSET_DIR}:"${NODEJS_DIR}":${PATH}
        if [ ! -e "$NODEJS_DIR" ]; then
            toolset_dl nodejs nodejs_nt-x86
            if [ ! -e "$NODEJS_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    OSX*)
        export NODEJS_DIR="${HAM_TOOLSET_DIR}/osx-x86/"
        export NODEJS_GLOBAL_MODULES_DIR="${NODEJS_DIR}lib/node_modules"
        export PATH=${HAM_TOOLSET_DIR}:"${NODEJS_DIR}/bin":${PATH}
        if [ ! -e "$NODEJS_DIR" ]; then
            toolset_dl nodejs nodejs_osx-x86
            if [ ! -e "$NODEJS_DIR" ]; then
                echo "E/osx-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        export NODE_PATH=$NODEJS_DIR/lib/node_modules
        ;;
    LINUX*)
        export NODEJS_DIR="${HAM_TOOLSET_DIR}/${HAM_BIN_LOA}/"
        export NODEJS_GLOBAL_MODULES_DIR="${NODEJS_DIR}lib/node_modules"
        export PATH=${HAM_TOOLSET_DIR}:"${NODEJS_DIR}/bin":"${NODEJS_DIR}/lib":${PATH}
        if [ ! -e "$NODEJS_DIR" ]; then
            toolset_dl nodejs nodejs_${HAM_BIN_LOA}
            if [ ! -e "$NODEJS_DIR" ]; then
                echo "E/osx-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        export NODE_PATH=$NODEJS_DIR/lib/node_modules
	      chmod +x "$NODEJS_DIR/bin/"*
	      chmod +x "$NODEJS_DIR/lib/"*
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- nodejs ------------------------
`node --version`
--- npm ---------------------------
`npm --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
