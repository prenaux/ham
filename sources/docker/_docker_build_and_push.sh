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
  echo "usage: ${0##*/} IMAGE_NAME|all"
  echo ""
  echo "example:"
  echo "  ./${0##*/} all"
  echo "  ./${0##*/} ham-tall-stack"
  echo ""
  exit 1
}

function build_and_push() {
  IMAGE_NAME="$1"
  if [ "$IMAGE_NAME" == ham-base ]; then
    (set -x ; ./_docker_build.sh nocache latest "$IMAGE_NAME")
  else
    (set -x ; ./_docker_build.sh latest "$IMAGE_NAME")
  fi
  (set -x ; ./_docker_push.sh latest "$IMAGE_NAME")
}

IMAGE_NAME=
if [ -n "$1" ]; then
  IMAGE_NAME=$1
  shift
else
  log_error "IMAGE_NAME not specified."
  usage
fi

if [ "$IMAGE_NAME" == "all" ]; then
  build_and_push ham-base
  build_and_push ham-nodejs
  build_and_push ham-tall-stack
else
  build_and_push "$IMAGE_NAME"
fi
