#!/bin/bash

# toolset
export HAM_TOOLSET=PYTHON
export HAM_TOOLSET_VER=36
export HAM_TOOLSET_NAME=python_36
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/python_36"

# path setup
case $HAM_OS in
    NT*)
        # Assumes the python 3.6 installer has run first and installed in the
        # default folder
        export PYTHON_DIR="$HOME/AppData/Local/Programs/Python/Python36"
        export PATH=${PYTHON_DIR}:${PYTHON_DIR}/DLLs:${PYTHON_DIR}/Scripts:${PATH}
        if [ ! -e "$PYTHON_DIR" ]; then
            echo "E/Python3.6 must be installed with the standard installer in the default folder"
            return 1
        fi
        alias python3=${PYTHON_DIR}/python.exe
        # I have no word for how insane the python 'path handling' works on
        # Windows... but we have to deal with it anyway...
        export PYTHON3_BINDIR="$HOME/AppData/Roaming/Python/Python36/Scripts/"
        export PATH=$PYTHON3_BINDIR:$PATH
        ;;
    OSX*)
        if [ ! -e "/usr/local/bin/pip3.6" ]; then
            echo "I/pip not found, installing..."
            sudo easy_install pip
        fi
        alias python=python3
        export PYTHON3_BINDIR=$HOME/Library/Python/3.6/bin
        export PATH=$PYTHON3_BINDIR:$PATH
        ;;
    LINUX*)
        true # Assume its already available and default on Linux
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- python_36 ------------------------
`python3 --version 2>&1`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
