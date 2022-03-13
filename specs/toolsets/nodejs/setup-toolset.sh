#!/bin/bash

# These are needed by gyp to build native nodejs modules
toolset_import_once default || return 1
toolset_import_once python_36 || return 1

# toolset
export HAM_TOOLSET=NODEJS
export HAM_TOOLSET_NAME=nodejs
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/nodejs"

# path setup
case $HAM_OS in
    NT*)
        toolset_check_and_dl_ver nodejs nt-x86 v10_16 || return 1
        export NODEJS_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export NODEJS_GLOBAL_MODULES_DIR="${NODEJS_DIR}/node_modules"
        export PATH=${HAM_TOOLSET_DIR}:"${NODEJS_DIR}":"${NODEJS_DIR}/bin":${PATH}
        export NODE_PATH="$NODEJS_DIR/lib/node_modules"
        ;;
    OSX*)
        if [ -z `which node` -o -z `which npm` ]; then
            echo "W/Couldn't find node, will try to install it with brew."
            # reinstall and link to make sure that a boched install works
            ham-brew reinstall node@16
            ham-brew link --overwrite node@16
        fi
        export NODEJS_GLOBAL_MODULES_DIR="/usr/local/lib/node_modules"
        if [ ! -d "$NODEJS_GLOBAL_MODULES_DIR" ]; then
            export NODEJS_GLOBAL_MODULES_DIR="/opt/homebrew/lib/node_modules"
        fi
        export PATH=${HAM_TOOLSET_DIR}:${PATH}
        export NODE_PATH=$NODEJS_GLOBAL_MODULES_DIR
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

# Install any missing global node tools
npm-install-global-deps

VER="--- nodejs ------------------------
`node --version`
--- npm ---------------------------
`npm --version`
--- yarn --------------------------
`yarn --version`
--- esbuild -----------------------
`esbuild --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
