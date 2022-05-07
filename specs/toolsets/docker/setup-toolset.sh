#!/bin/bash

# toolset
export HAM_TOOLSET=DOCKER
export HAM_TOOLSET_NAME=docker
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/docker"

case "$HAM_BIN_LOA" in
    lin-x64)
        if [ -z `where_inpath docker` ]; then
            #
            # See: https://docs.docker.com/engine/installation/linux/ubuntu/#install-using-the-repository
            #
            sudo true # Ask for the sudo pwd first
            sudo apt-get update && sudo apt-get install -y apt-transport-https ca-certificates curl software-properties-common
            curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
            sudo apt-key fingerprint 0EBFCD88 | grep docker@docker.com || exit 1
            sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"
            sudo apt-get update
            sudo apt-get install -y docker-ce
            sudo docker run --rm hello-world
            #
            # To uninstall
            #  sudo apt-get remove -y docker-ce && sudo apt-get autoremove
            #
        fi
    ;;

    *)
        complain docker_toolset "Unsupported arch '$HAM_BIN_LOA'."
        return 1;
        ;;
esac
pathenv_add "$HAM_TOOLSET_DIR"

VER="--- docker ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`docker --version`"
    if [ $? != 0 ]; then
        echo "E/Can't get version."
        return 1
    fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
