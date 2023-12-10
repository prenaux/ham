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
  echo "usage: ${0##*/} TAG_NAME image_name"
  echo ""
  echo "example:"
  echo "  ./${0##*/} latest ham-base"
  echo ""
  exit 1
}

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

REPO_NAME=${HAM_DOCKER_REPO_NAME:-prenaux}
TAG="${REPO_NAME}/${IMAGE_NAME}:${TAG_NAME}"
log_info "Pushing '${IMAGE_NAME}' image tagged '$TAG' to repo '${REPO_NAME}'."
(
  set -x
  docker tag "${IMAGE_NAME}" "${TAG}"
  docker push "${TAG}"
)
log_success "Successfully pushed '$IMAGE_NAME' image with tag '$TAG_NAME'."
