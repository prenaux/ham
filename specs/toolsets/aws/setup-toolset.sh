#!/bin/bash
. ham-toolset-import.sh python_27
. ham-toolset-import.sh python_36

# toolset
export HAM_TOOLSET=AWS
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=aws
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/${HAM_TOOLSET_NAME}"

# path setup
case $HAM_OS in
    NT)
        ## Our embedded python with pip and eb packaged
        export AWS_PYTHON_DIR="${HAM_TOOLSET_DIR}/nt-x86/python"
        # dl if missing
        if [ ! -e "$AWS_PYTHON_DIR"  ]; then
            toolset_dl aws aws_nt-x86
            if [ ! -e "$AWS_PYTHON_DIR" ]; then
                echo "aws nt-x86/python folder doesn't exist in the toolset"
                return 1
            fi
        fi
        export PATH=${AWS_PYTHON_DIR}:${AWS_PYTHON_DIR}/DLLs:${PATH}
        ;;
    OSX)
        if [ ! -e "$PYTHON2_BINDIR/eb" ]; then
            echo "I/eb not found, installing..."
            pip install awsebcli --upgrade --user
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
`aws --version`
`aws-eb --version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
