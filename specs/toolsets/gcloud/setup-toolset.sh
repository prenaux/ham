#!/bin/bash

export HAM_TOOLSET=GCLOUD
export HAM_TOOLSET_NAME=gcloud
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/gcloud"

export GCLOUD_DIR="${HAM_TOOLSET_DIR}/${HAM_BIN_LOA}"
export GCLOUD_BIN_DIR="${GCLOUD_DIR}/bin"

GCLOUD_DL_VER=455.0.0
GCLOUD_DL_TAG=$(echo ${GCLOUD_DL_VER} | tr '.' '_')

if [ ! -f "$GCLOUD_DIR/$GCLOUD_DL_TAG" ]; then
  echo "W/Couldn't find gcloud ${GCLOUD_DL_VER}, installing from the dist package..."

  GCLOUD_DL_NAME="google-cloud-sdk"
  case "$HAM_BIN_LOA" in
    osx-x64)
      GCLOUD_DL_FN="google-cloud-cli-${GCLOUD_DL_VER}-darwin-x86_64.tar.gz"
      ;;
    osx-arm64)
      GCLOUD_DL_FN="google-cloud-cli-${GCLOUD_DL_VER}-darwin-arm.tar.gz"
      ;;
    lin-*)
      GCLOUD_DL_FN="google-cloud-cli-${GCLOUD_DL_VER}-linux-x86_64.tar.gz"
      ;;
    *)
      echo "Unsupported platform '$HAM_BIN_LOA'."
      return 1
      ;;
  esac
  GCLOUD_DL_URL="https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/${GCLOUD_DL_FN}"
  (
    set -ex
    rm -Rf "$GCLOUD_DIR"
    ham-dl-file "${GCLOUD_DL_FN}" "$GCLOUD_DL_URL"
    ham-unpack "${GCLOUD_DL_FN}" "${HAM_TOOLSET_DIR}"
    mv "${HAM_TOOLSET_DIR}/${GCLOUD_DL_NAME}" "${GCLOUD_DIR}"
    rm "${GCLOUD_DL_FN}"
  )

  if [ ! -f "$GCLOUD_BIN_DIR/gcloud" ]; then
    echo "E/'$GCLOUD_BIN_DIR/gcloud' not found after unpacking the archive '${GCLOUD_DL_URL}'."
    return 1
  fi

  # Output the tag file
  (
    set -x
    echo "$GCLOUD_DL_VER" >"$GCLOUD_DIR/$GCLOUD_DL_TAG"
  )

  echo "I/GCLOUD_BIN_DIR: ${GCLOUD_BIN_DIR}"
fi

pathenv_add "${GCLOUD_DIR}"
pathenv_add "${GCLOUD_BIN_DIR}"

VER="--- gcloud ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(gcloud --version | grep SDK)"; then
    echo "E/Can't get version."
    return 1
  fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
