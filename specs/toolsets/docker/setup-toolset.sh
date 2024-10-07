#!/bin/bash

# toolset
export HAM_TOOLSET=DOCKER
export HAM_TOOLSET_NAME=docker
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/docker"

case "$HAM_BIN_LOA" in
  lin-x64)
    if [ -z "$(where_inpath docker)" ]; then
      log_info "Adding Docker repository to Apt sources..."
      (
        set -x
        sudo apt-get update -y
        sudo apt-get install -y ca-certificates curl
        sudo install -m 0755 -d /etc/apt/keyrings
        sudo curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
        sudo chmod a+r /etc/apt/keyrings/docker.asc
        sudo apt-get update -y
      )

      if ! grep -q "^deb .*download.docker.com/linux/ubuntu" /etc/apt/sources.list.d/docker.list 2>/dev/null; then
        log_info "Adding Docker repo to apt sources.list."
        echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] https://download.docker.com/linux/ubuntu \
          $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | sudo tee /etc/apt/sources.list.d/docker.list >/dev/null
      else
        log_info "Docker repo already added to apt sources.list."
      fi

      ham-apt-get-install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
    fi
    ;;

  *)
    complain docker_toolset "Unsupported arch '$HAM_BIN_LOA'."
    return 1
    ;;
esac
pathenv_add "$HAM_TOOLSET_DIR"

VER="--- docker ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(docker --version)"; then
    echo "E/Can't get version."
    return 1
  fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
