#!/bin/bash -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

DEPLOY_MODULE=$(basename "$SCRIPT_DIR")
CONTAINER_REGISTRY=asia-docker.pkg.dev/we3d-games/w3-docker

if [ -z "$BRANCH" ]; then
  BRANCH="$(git-head-info branch)"
fi

docker tag "$DEPLOY_MODULE:$BRANCH" "$CONTAINER_REGISTRY/$DEPLOY_MODULE:$BRANCH"
docker push "$CONTAINER_REGISTRY/$DEPLOY_MODULE:$BRANCH"
