#!/bin/bash

# toolset
export HAM_TOOLSET=PYTHON
export HAM_TOOLSET_NAME=python_27
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/python_27"

# path setup
case $HAM_OS in
    NT*)
        export PYTHON_DIR="${HAM_TOOLSET_DIR}/nt-x86/"
        if [ ! -e "$PYTHON_DIR" ]; then
            toolset_dl python_27 python_27_nt-x86
            if [ ! -e "$PYTHON_DIR" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        export PYTHON="${PYTHON_DIR}/python.exe"
        export PYTHON2_BINDIR="${PYTHON_DIR}/Scripts"
        pathenv_add "${PYTHON_DIR}"
        pathenv_add "${PYTHON_DIR}/DLLs"
        pathenv_add "${PYTHON_DIR}/Scripts"
        ;;
    OSX*)
        ln -s /usr/bin/python2.7 "${HAM_HOME}/bin/osx-x86/python2"
        if [ ! -e "/usr/local/bin/pip" ]; then
            echo "I/pip not found, installing..."
            sudo easy_install pip
        fi
        alias python=python2
        export PYTHON2_BINDIR=$HOME/Library/Python/2.7/bin
        pathenv_add "$PYTHON2_BINDIR"
        ;;
    LINUX*)
        true # Assume its already available and default on Linux
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- python_27 ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`python --version 2>&1`"
    if [ $? != 0 ]; then
      echo "E/Can't get version."
      return 1
    fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
