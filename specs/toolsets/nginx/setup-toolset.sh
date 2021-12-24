#!/bin/bash

# toolset
export HAM_TOOLSET=NGINX
export HAM_TOOLSET_VER=192
export HAM_TOOLSET_NAME=nginx
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/nginx"

# path setup
case $HAM_OS in
    NT*)
        export NGINX_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH="${NGINX_DIR}":"${HAM_TOOLSET_DIR}":${PATH}
        if [ ! -e "$NGINX_DIR/nginx.exe" ]; then
            toolset_dl nginx nginx_nt-x86
            if [ ! -e "$NGINX_DIR/nginx.exe" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    OSX*)
        if [ ! -x "$(command -v nginx)" ]; then
            echo "I/nginx not found, using brew to install it..."
            ham-brew install nginx
        fi
        if [ ! -x "$(command -v certbot)" ]; then
            echo "I/certbox not found, using brew to install it..."
            brew install certbot
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- nginx ------------------------
`nginx -v 2>&1`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
