#!/bin/bash -e
usage() {
  echo "usage: ${0##*/}"
  echo ""
  echo "  Builds the docker image."
  exit 1
}

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cd "$SCRIPT_DIR"
export DEPLOY_MODULE=$(basename $(pwd))
# DEPLOY_DIR must be within the repo in order for github action to work
export DEPLOY_DIR="$SCRIPT_DIR/_deploy"

if [ -z "$BRANCH" ]; then
  export BRANCH="$(git-head-info branch)"
fi

# Build
echo "I/Building artifacts..."
(
  export BUILD=ra
  export HAM_TARGET_BIN_LOA=lin-x64
  ham -T./_ham_project ham-test-thrift-server
)

# Copy the files
echo "I/Copying to deployment folder: $DEPLOY_DIR"
BUILD=${BUILD:-ra}
DEPLOY_EXE="bin/$(ham-cppm-bin-filepath exe ham-test-thrift-server_${BUILD})"
if [ ! -e "${SCRIPT_DIR}/$DEPLOY_EXE" ]; then
  echo "E/Can't find build artifact '$DEPLOY_EXE'."
  exit 1
fi

# Create the deploy folder
(
  set -x
  rm -Rf "$DEPLOY_DIR"
  mkdir -p "$DEPLOY_DIR"
)

# List of specific files to be exported, relative to $SCRIPT_DIR
EXPORTED_FILES=(
  "$DEPLOY_EXE"
  "_docker_incontainer_run.sh"
)
for FILE in ${EXPORTED_FILES[@]}; do
  # Copy file to destination
  if [ -f "$SCRIPT_DIR/$FILE" ]; then
    echo "I/Copying file $FILE to deployment folder"
    mkdir -p $(dirname "$DEPLOY_DIR/$FILE")
    cp "$SCRIPT_DIR/$FILE" "$DEPLOY_DIR/$FILE"
  else
    echo "E/Can't find artifact '$SCRIPT_DIR/$FILE'."
    exit 1
  fi
done

docker build . -t ${DEPLOY_MODULE}:latest -t ${DEPLOY_MODULE}:${BRANCH} \
  -f "${DEPLOY_MODULE}.dockerfile" \
  --build-arg DEPLOY_EXE=${DEPLOY_EXE}

# Cleanup the deploy folder
echo "I/Cleanup deploy folder."
(
  set -x
  rm -Rf "$DEPLOY_DIR"
)

echo "I/Done."
