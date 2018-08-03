#!/bin/bash

toolset_import python_27
if [ $? != 0 ]; then return 1; fi
toolset_import repos
if [ $? != 0 ]; then return 1; fi
toolset_import nginx
if [ $? != 0 ]; then return 1; fi

# toolset
export HAM_TOOLSET=KUBE
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=kubernetes
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/kube"

# path setup
case $HAM_OS in
    NT*)
        export KUBE_HOME="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH="${KUBE_HOME}/bin":"${KUBE_HOME}/google-cloud-sdk/bin/":${PATH}
        if [ ! -e "$KUBE_HOME" ]; then
            toolset_dl kube kube_nt-x86
            if [ ! -e "$KUBE_HOME" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

# path
export HELM_HOME="$WORK/Server/minikube/helm"
export MINIKUBE_HOME="$WORK/Server/minikube"
export PATH="${HAM_TOOLSET_DIR}":${PATH}

VER="--- kubernetes ---------------
`gcloud --version | grep 'Google Cloud SDK'`
`kubectl version | grep 'Client Version:'`
`minikube version`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
