#!/bin/bash -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

DEPLOY_MODULE=$(basename "$SCRIPT_DIR")
CONTAINER_REGISTRY=asia-docker.pkg.dev/we3d-games/w3-docker
SERVICE_PREFIX=ham-test-thrift
SERVICE_SUFFIX=dev # we have a suffix to make sure we never have a dash at the end
TAIL_CHARS=43      # Should be: 63 - len($SERVICE_PREFIX) - len($SERVICE_SUFFIX) - 2
# TODO: configurable region?
REGION=asia-southeast1

if [ -z "$BRANCH" ]; then
  BRANCH="$(git-head-info branch)"
fi
echo "I/BRANCH: $BRANCH"

# Service name "must use only lowercase alphanumeric characters and dashes,
# cannot begin or end with a dash, and cannot be longer than 63 characters."
SERVICE="${SERVICE_PREFIX}-$(echo "${BRANCH}" | tr ' .' '-' | tr -dc '[:alnum:]-' | tr '[:upper:]' '[:lower:]' | tail -c ${TAIL_CHARS})-${SERVICE_SUFFIX}"
echo "I/Service name: $SERVICE"

gcloud run deploy "$SERVICE" \
  --image "$CONTAINER_REGISTRY/$DEPLOY_MODULE:$BRANCH" \
  --allow-unauthenticated \
  --project=we3d-games \
  --region=$REGION \
  --port="${PORT:-40990}"

# If we are running in a github action, fetch the service URL and output it to the CI build summary
if [ -f "$GITHUB_STEP_SUMMARY" ]; then
  SERVICE_URL=$(gcloud run services describe "$SERVICE" --platform managed --region $REGION --format 'value(status.url)')
  echo "- Deployment URL: $SERVICE_URL" >>"$GITHUB_STEP_SUMMARY"
fi
