#!/bin/bash -e
if [ ! -e "$DEPLOY_EXE" ]; then
  echo "E/DEPLOY_EXE not found '$DEPLOY_EXE'."
  exit 1
fi
if [ -z "$PORT" ]; then
  echo "E/PORT not set '$PORT'."
  exit 1
fi
"$DEPLOY_EXE" -Dport="${PORT}"
