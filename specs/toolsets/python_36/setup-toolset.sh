#!/bin/bash

# toolset
export HAM_TOOLSET=PYTHON
export HAM_TOOLSET_VER=36
export HAM_TOOLSET_NAME=python_36
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/python_36"

# path setup
case $HAM_OS in
    NT*)
        # Look for python in default install destination...
        export PYTHON_DIR="$HOME/AppData/Local/Programs/Python/Python36"
        export PATH=${PYTHON_DIR}:${PYTHON_DIR}/DLLs:${PYTHON_DIR}/Scripts:${PATH}
        if [ ! -e "$PYTHON_DIR" ]; then
            echo "I/Downloading Python 3.6..."
            dl_file https://www.python.org/ftp/python/3.6.8/python-3.6.8-amd64.exe -O"$TMP/python-3.6.8-amd64.exe"
            errcheck $? python_36 "E/Can't download python 3"
            echo "I/Installing Python 3.6..."
            "$TMP/python-3.6.8-amd64.exe" -quiet
            errcheck $? python_36 "E/Can't install python 3"
            rm "$TMP/python-3.6.8-amd64.exe"
        fi
        alias python3=${PYTHON_DIR}/python.exe
        # I have no word for how insane the python 'path handling' works on
        # Windows... but we have to deal with it anyway...
        export PYTHON3_BINDIR="$HOME/AppData/Roaming/Python/Python36/Scripts/"
        export PATH=$PYTHON3_BINDIR:$PATH
        ;;

    OSX*)
        if [ ! -e "/usr/local/bin/pip3" ]; then
            echo "I/pip not found, installing..."
            brew install python3
            errcheck $? python_36 "E/Can't install python 3"
            if [ ! -e "/usr/local/bin/pip3" ]; then
                errcheck $? python_36 "E/Can't install pip 3"
            fi
        fi
        alias python=python3
        if [ -e "$HOME/Library/Python/3.8/bin" ]; then
            export PYTHON3_BINDIR=$HOME/Library/Python/3.8/bin
        elif [ -e "$HOME/Library/Python/3.7/bin" ]; then
            export PYTHON3_BINDIR=$HOME/Library/Python/3.7/bin
        elif [ -e "$HOME/Library/Python/3.6/bin" ]; then
            export PYTHON3_BINDIR=$HOME/Library/Python/3.6/bin
        else
            echo "E/Can't find a python library folder in $HOME/Library/Python/"
            return 1
        fi
        export PATH=$PYTHON3_BINDIR:$PATH
        ;;

    LINUX*)
        export PATH=~/.local/bin:${PATH}
        ;;

    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- python3 --------------------------
`python3 --version 2>&1`"
if [ $? != 0 ]; then
    echo "E/Can't get python3 version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

VER="--- pip3 -----------------------------
`pip3 --version 2>&1`"
if [ $? != 0 ]; then
    echo "E/Can't get pip3 version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
