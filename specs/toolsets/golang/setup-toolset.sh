#!/bin/bash

# toolset
export HAM_TOOLSET=GOLANG
export HAM_TOOLSET_NAME=golang
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/golang"

# We use a tag file to mark the version so that upgrades are automatically
# handled when we update the version number
GOLANG_DL_VER=1.22.1
GOLANG_DL_TAG=$(echo v${GOLANG_DL_VER} | tr '.' '_')
export GOLANG_DIR="${HAM_TOOLSET_DIR}/${HAM_BIN_LOA}/go"
export GOLANG_BIN_DIR="${GOLANG_DIR}/bin"

# Should be set per project in _ham_project, but this is a reasonable default
export GOPATH="${HAM_TOOLSET_DIR}/dist/"
mkdir -p "$GOPATH"

pathenv_add "${HAM_TOOLSET_DIR}"
if [ ! -f "$GOLANG_DIR/$GOLANG_DL_TAG" ]; then
  echo "W/Couldn't find golang, installing from the go.dev/dl dist package..."
  case "$HAM_BIN_LOA" in
    lin-x64)
      GOLANG_DL_NAME="go${GOLANG_DL_VER}.linux-amd64"
      GOLANG_DL_FN="${GOLANG_DL_NAME}.tar.gz"
      ;;
    osx-arm64)
      GOLANG_DL_NAME="go${GOLANG_DL_VER}.darwin-arm64"
      GOLANG_DL_FN="${GOLANG_DL_NAME}.tar.gz"
      ;;
    osx-amd64)
      GOLANG_DL_NAME="go${GOLANG_DL_VER}.darwin-amd64"
      GOLANG_DL_FN="${GOLANG_DL_NAME}.tar.gz"
      ;;
    *)
      complain golang_toolset "Unsupported arch '$HAM_BIN_LOA'."
      return 1
      ;;
  esac
  # Ex: https://go.dev/dl/go1.21.6.linux-arm64.tar.gz
  GOLANG_DL_URL="https://go.dev/dl/${GOLANG_DL_FN}"
  (
    set -ex
    rm -Rf "$GOLANG_DIR"
    mkdir -p "$GOLANG_DIR"
    cd "$GOLANG_DIR"/..
    ham-dl-file "${GOLANG_DL_FN}" "$GOLANG_DL_URL"
    ham-unpack "${GOLANG_DL_FN}" "${GOLANG_DIR}/.."
    rm "${GOLANG_DL_FN}"
  )
  if [ ! -f "$GOLANG_BIN_DIR/go" ]; then
    echo "E/'go' not found after unpacking the archive '${GOLANG_DL_URL}'."
    return 1
  fi
  # Output the tag file
  (
    set -x
    echo "$GOLANG_DL_VER" >"$GOLANG_DIR/$GOLANG_DL_TAG"
  )
  echo "I/GOLANG_BIN_DIR: ${GOLANG_BIN_DIR}"
fi

chmod +x "$GOLANG_BIN_DIR/"*
pathenv_add "${GOLANG_BIN_DIR}"

VER="--- golang ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(go version)"; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
