#!/bin/bash
. ham-toolset-import.sh repos
. ham-toolset-import.sh python_36

# toolset
export HAM_TOOLSET=AWS
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=aws
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}"

# path setup
case $HAM_OS in
    NT)
        if [ ! -e "$PYTHON3_BINDIR/eb" ]; then
            echo "I/eb not found, installing..."
            pip3 install awsebcli --upgrade --user
            errcheck $? aws "E/Can't pip3 install awsebcli."
        fi
        if [ ! -e "$PYTHON3_BINDIR/aws" ]; then
            echo "I/aws not found, installing..."
            pip3 install awscli --upgrade --user
            errcheck $? aws "E/Can't pip3 install awscli."
        fi
        ;;
    OSX)
        if [ ! -e "$PYTHON3_BINDIR/eb" ]; then
            echo "I/eb not found, installing..."
            pip3 install awsebcli --upgrade --user
        fi
        if [ ! -e "$PYTHON3_BINDIR/aws" ]; then
            echo "I/aws not found, installing..."
            pip3 install awscli --upgrade --user
        fi
        ;;
    LINUX)
        ## Assume eb & aws are already on the path somehow...
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
export PATH=$HAM_TOOLSET_DIR:${PATH}

VER="--- aws ---------------------------
`aws --version`"
if [ $? != 0 ]; then
    echo "E/Can't get aws-cli version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

VER="--- aws-eb ------------------------
`aws-eb --version`"
if [ $? != 0 ]; then
    echo "E/Can't get aws-eb version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
