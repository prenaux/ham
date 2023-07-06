#!/bin/bash

# toolset
export HAM_TOOLSET=PYTHON
export HAM_TOOLSET_NAME=python_3
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/python_3"

# path setup
case $HAM_OS in
    NT*)
        PYINSTALL_VERDIR=Python310
        PYINSTALL_VERSION=3.10.11
        # Look for python in default install destination...
        export PYTHON3_DIR="$HOME/AppData/Local/Programs/Python/$PYINSTALL_VERDIR"
        if [ ! -e "$PYTHON3_DIR" ] || [ ! -e "$PYTHON3_DIR/python.exe" ]; then
            echo "I/Downloading Python ${PYINSTALL_VERSION}..."
            # PYINSTALL_EXE_NAME=python-3_10_11-amd64.exe
            # dl_file "$TEMPDIR/$PYINSTALL_EXE_NAME" https://cdn2.talansoft.com/ftp/toolsets/$PYINSTALL_EXE_NAME
            PYINSTALL_EXE_NAME=python-${PYINSTALL_VERSION}-amd64.exe
            dl_file "$TEMPDIR/$PYINSTALL_EXE_NAME" https://www.python.org/ftp/python/$PYINSTALL_VERSION/$PYINSTALL_EXE_NAME
            errcheck $? python_3 "E/Can't download python 3" || return 1
            echo "I/Installing Python ${PYINSTALL_VERSION}..."
            (set -x ; "$TEMPDIR/$PYINSTALL_EXE_NAME" -quiet InstallAllUsers=0 Include_launcher=0)
            errcheck $? python_3 "E/Can't install python 3" || return 1
            if [ ! -e "${PYTHON3_DIR}/python.exe" ]; then
                echo "W/Can't find exe after install, repairing..."
                (set -x ; "$TEMPDIR/$PYINSTALL_EXE_NAME" -repair -quiet)
                errcheck $? python_3 "E/Can't repair python 3" || return 1
            fi
            rm -f "$TEMPDIR/$PYINSTALL_EXE_NAME"
        fi
        pathenv_add ${PYTHON3_DIR}/Scripts
        pathenv_add ${PYTHON3_DIR}/DLLs
        pathenv_add ${PYTHON3_DIR}
        # I have no word for how insane the python 'path handling' works on
        # Windows... but we have to deal with it anyway...
        export PYTHON3_BINDIR="$HOME/AppData/Roaming/Python/$PYINSTALL_VERDIR/Scripts/"
        pathenv_add "$PYTHON3_BINDIR"
        ;;

    OSX*)
        export PYTHON3_VERSION=3.10
        ham-brew-install python@${PYTHON3_VERSION} "bin/python${PYTHON3_VERSION}" || return 1
        export PYTHON3_HOME=`ham-brew-installdir python@${PYTHON3_VERSION}`
        export PYTHON3_BINDIR="$HOME/Library/Python/${PYTHON3_VERSION}/bin"
        # Might not exist the first time we install python...
        mkdir -p "$PYTHON3_BINDIR"
        pathenv_add "$PYTHON3_BINDIR"
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
