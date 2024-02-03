#!/bin/bash

case "$BUILD_TARGET" in
  "" | nt-* | osx-* | lin-*)
    NODEJS_CAN_NODE_MODULES=1
    ;;
  *)
    NODEJS_CAN_NODE_MODULES=0
    ;;
esac

if [ "$NODEJS_CAN_NODE_MODULES" = "1" ]; then
  # These are needed by gyp to build native nodejs modules
  toolset_import_once python_3 || return 1
  toolset_import_once default || return 1
fi

# toolset
export HAM_TOOLSET=NODEJS
export HAM_TOOLSET_NAME=nodejs
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/nodejs"

# We use a tag file to mark the version so that upgrades are automatically handled when we update the version number
NODEJS_DL_VER=v20.11.0
NODEJS_DL_TAG=$(echo ${NODEJS_DL_VER} | tr '.' '_')
export NODEJS_DIR="${HAM_TOOLSET_DIR}/${HAM_BIN_LOA}"
case "$HAM_BIN_LOA" in
  nt-x86)
    # Absolutely obnoxious, why would you use a different folder structure only on Windows??
    export NODEJS_BIN_DIR="${NODEJS_DIR}"
    export NODEJS_GLOBAL_MODULES_DIR="${NODEJS_DIR}/node_modules"
    if [[ "${GITHUB_ACTIONS}" == "true" ]]; then
      # Because of course GITHUB_ACTIONS do it even more different...
      export NODEJS_GLOBAL_MODULES_BIN_DIR="c:/npm/prefix"
    else
      export NODEJS_GLOBAL_MODULES_BIN_DIR="$NODEJS_BIN_DIR"
    fi
    ;;
  *)
    export NODEJS_BIN_DIR="${NODEJS_DIR}/bin"
    export NODEJS_GLOBAL_MODULES_DIR="${NODEJS_DIR}/lib/node_modules"
    export NODEJS_GLOBAL_MODULES_BIN_DIR="$NODEJS_BIN_DIR"
    ;;
esac
export NODE_PATH=$NODEJS_GLOBAL_MODULES_DIR

pathenv_add "${HAM_TOOLSET_DIR}"
if [ ! -f "$NODEJS_DIR/$NODEJS_DL_TAG" ]; then
  echo "W/Couldn't find node '$NODEJS_DL_VER', installing from the nodejs.org dist package..."
  case "$HAM_BIN_LOA" in
    nt-x86)
      NODEJS_DL_NAME="node-${NODEJS_DL_VER}-win-x64"
      NODEJS_DL_FN="${NODEJS_DL_NAME}.7z"
      ;;
    osx-x64)
      NODEJS_DL_NAME="node-${NODEJS_DL_VER}-darwin-x64"
      NODEJS_DL_FN="${NODEJS_DL_NAME}.tar.gz"
      ;;
    osx-arm64)
      NODEJS_DL_NAME="node-${NODEJS_DL_VER}-darwin-arm64"
      NODEJS_DL_FN="${NODEJS_DL_NAME}.tar.gz"
      ;;
    lin-x64)
      NODEJS_DL_NAME="node-${NODEJS_DL_VER}-linux-x64"
      NODEJS_DL_FN="${NODEJS_DL_NAME}.tar.gz"
      ;;
    *)
      complain nodejs_toolset "Unsupported arch '$HAM_BIN_LOA'."
      return 1
      ;;
  esac
  NODEJS_DL_URL="https://nodejs.org/dist/${NODEJS_DL_VER}/${NODEJS_DL_FN}"
  (
    set -ex
    rm -Rf "$NODEJS_DIR"
    ham-dl-file "${NODEJS_DL_FN}" "$NODEJS_DL_URL"
    ham-unpack "${NODEJS_DL_FN}" "${HAM_TOOLSET_DIR}"
    mv "${HAM_TOOLSET_DIR}/${NODEJS_DL_NAME}" "${NODEJS_DIR}"
    rm "${NODEJS_DL_FN}"
  )
  if [ ! -f "$NODEJS_BIN_DIR/node" ]; then
    echo "E/'node' not found after unpacking the archive '${NODEJS_DL_URL}'."
    return 1
  fi
  if [ ! -f "$NODEJS_BIN_DIR/npm" ]; then
    echo "E/'npm' not found after unpacking the archive '${NODEJS_DL_URL}'."
    return 1
  fi
  # Output the tag file
  (
    set -x
    echo "$NODEJS_DL_VER" >"$NODEJS_DIR/$NODEJS_DL_TAG"
  )
  echo "I/NODEJS_BIN_DIR: ${NODEJS_BIN_DIR}"
  echo "I/NODEJS_GLOBAL_MODULES_DIR: ${NODEJS_GLOBAL_MODULES_DIR}"
  echo "I/NODEJS_GLOBAL_MODULES_BIN_DIR: ${NODEJS_GLOBAL_MODULES_BIN_DIR}"
fi

chmod +x "$NODEJS_BIN_DIR/"*
pathenv_add "${NODEJS_BIN_DIR}"
pathenv_add "${NODEJS_GLOBAL_MODULES_DIR}"
pathenv_add "${NODEJS_GLOBAL_MODULES_BIN_DIR}"

if [ "$NODEJS_CAN_NODE_MODULES" = "1" ]; then
  # Install any missing global node tools
  npm-install-global-deps
else
  echo "W/nodejs toolset: BUILD_TARGET set to '$BUILD_TARGET' we cannot build or install node modules."
fi

VER="--- nodejs ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(node --version)
--- npm ---------------------------
$(npm --version)
--- yarn --------------------------
$(yarn --version)
--- esbuild -----------------------
$(esbuild --version)"; then
    echo "E/Can't get version."
    return 1
  fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
