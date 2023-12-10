#!/bin/bash -e
#===== PRELUDE BEGIN ===========
if [[ -z "$HAM_HOME" ]]; then echo "E/HAM_HOME not set !"; exit 1; fi
# shellcheck disable=SC2034
SCRIPT_NAME=$(basename "$0")
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
. "$HAM_HOME/bin/ham-bash-setenv.sh"
#===== PRELUDE TOOLSETS ========
export HAM_NO_VER_CHECK=1
toolset_import_once repos docker > /dev/null
#===== PRELUDE END =============
cd "${SCRIPT_DIR}"

usage() {
  echo "usage: ${0##*/} (quiet) command TAG_NAME image_name|all"
  echo ""
  echo "example:"
  echo "  ./${0##*/} latest all"
  echo "  ./${0##*/} latest ham-base"
  echo ""
  exit 1
}

DOCKER_PARAMS=()
if [ "$1" == "quiet" ]; then
  DOCKER_PARAMS=("${DOCKER_PARAMS[@]}" --q)
  shift
fi

if [ "$1" == "nocache" ]; then
  DOCKER_PARAMS=("${DOCKER_PARAMS[@]}" --no-cache)
  shift
fi

TAG_NAME=
if [ -n "$1" ]; then
  TAG_NAME=$1
  shift
else
  log_error "TAG_NAME not specified."
  usage
fi

IMAGE_NAME=
if [ -n "$1" ]; then
  IMAGE_NAME=$1
  shift
else
  log_error "IMAGE_NAME not specified."
  usage
fi

# build_image IMAGE_NAME
function build_image() {
  IMAGE_NAME="$1"
  if [ -z "$IMAGE_NAME" ]; then
    echo "build_image: IMAGE_NAME not specified."
    usage
  fi

  DOCKERFILE_PATH=${SCRIPT_DIR}/${IMAGE_NAME}.dockerfile
  if [ ! -e "$DOCKERFILE_PATH" ]; then
    log_error "Can't find docker file '${DOCKERFILE_PATH}'."
    usage
  fi

  PARAMS=("${DOCKER_PARAMS[@]}")
  if [ "$IMAGE_NAME" != "ham-base" ]; then
    PARAMS=("${PARAMS[@]}" --pull=false)
  fi

  log_info "Building '$IMAGE_NAME' image with tag '$TAG_NAME'."
  (set -x ;
   # DOCKER_OPTS: https://stackoverflow.com/questions/24991136/docker-build-could-not-resolve-archive-ubuntu-com-apt-get-fails-to-install-a
   export DOCKER_OPTS="--dns 8.8.8.8 --dns 8.8.4.4" ;
   docker build "${PARAMS[@]}" -t ${IMAGE_NAME}:latest -t ${IMAGE_NAME}:${TAG_NAME} -f "${DOCKERFILE_PATH}" .)
  log_info "Successfully built '$IMAGE_NAME' image with tag '$TAG_NAME'."
}

if [ "$IMAGE_NAME" == "all" ]; then
  log_info "Building all the images..."
  build_image ham-base
  build_image ham-tall-stack
else
  build_image "$IMAGE_NAME"
fi

log_info "Done."
