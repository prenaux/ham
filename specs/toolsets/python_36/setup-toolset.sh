#!/bin/bash

# toolset
export HAM_TOOLSET=PYTHON
export HAM_TOOLSET_NAME=python_36
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/python_36"

# path setup
case $HAM_OS in
    NT*)
        # Look for python in default install destination...
        export PYTHON3_DIR="$HOME/AppData/Local/Programs/Python/Python36"
        if [ ! -e "$PYTHON3_DIR" ]; then
            echo "I/Downloading Python 3.6..."
            dl_file https://www.python.org/ftp/python/3.6.8/python-3.6.8-amd64.exe -O"$TMP/python-3.6.8-amd64.exe"
            errcheck $? python_36 "E/Can't download python 3"
            echo "I/Installing Python 3.6..."
            "$TMP/python-3.6.8-amd64.exe" -quiet
            errcheck $? python_36 "E/Can't install python 3"
            rm "$TMP/python-3.6.8-amd64.exe"
        fi
        pathenv_add ${PYTHON3_DIR}/Scripts
        pathenv_add ${PYTHON3_DIR}/DLLs
        pathenv_add ${PYTHON3_DIR}
        # I have no word for how insane the python 'path handling' works on
        # Windows... but we have to deal with it anyway...
        export PYTHON3_BINDIR="$HOME/AppData/Roaming/Python/Python36/Scripts/"
        pathenv_add "$PYTHON3_BINDIR"
        ;;

    OSX*)
        ham-brew-install python3 "bin/python3" || return 1
        export PYTHON3_HOME=`ham-brew-installdir python3`
        pathenv_add "$PYTHON3_HOME/bin"
        ;;

    LINUX*)
        export PATH=~/.local/bin:${PATH}
        ;;

    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

pathenv_add "$HAM_TOOLSET_DIR"

VER="--- python3 --------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`python3 --version 2>&1`
--- pip3 -----------------------------
`pip3 --version 2>&1`"
    if [ $? != 0 ]; then
        echo "E/Can't get python3 version."
        return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
