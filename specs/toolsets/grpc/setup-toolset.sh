#!/bin/bash

# toolset
export HAM_TOOLSET=GRPC
export HAM_TOOLSET_NAME=grpc
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/grpc"

case $HAM_BIN_LOA in
  osx-arm64)
    # we only have a x64 exe atm
    BIN_LOA=osx-x64
    ;;
  osx-x86 | lin-x64 | nt-x86)
    BIN_LOA=$HAM_BIN_LOA
    ;;
  *)
    echo "E/Toolset: Unsupported host HAM_BIN_LOA '$HAM_BIN_LOA'"
    return 1
    ;;
esac

# check and dl toolset
toolset_check_and_dl_ver grpc "${BIN_LOA}" v22_10_13 || return 1

# path
export GRPC_DIST_DIR="${HAM_TOOLSET_DIR}/${BIN_LOA}"
pathenv_add "${GRPC_DIST_DIR}"
pathenv_add "${GRPC_DIST_DIR}/protobuf-javascript/bin"
pathenv_add "${GRPC_DIST_DIR}/protoc/bin"
pathenv_add "${HAM_TOOLSET_DIR}"

VER="--- protobuf --------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  VER="$VER
$(protoc --version)"
  errcheck $? ${HAM_TOOLSET_NAME} "protobuf version check failed." || return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

VER="--- protoc-gen-js ---------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  VER="$VER
Location: $(where_inpath protoc-gen-js)"
  errcheck $? ${HAM_TOOLSET_NAME} "protoc-gen-js location check failed." || return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

VER="--- protoc-gen-grpc-web ---------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  VER="$VER
Location: $(where_inpath protoc-gen-grpc-web)"
  errcheck $? ${HAM_TOOLSET_NAME} "protoc-gen-grpc-web location check failed." || return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

VER="--- grpcurl --------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  VER="$VER
$(grpcurl --version 2>&1)"
  errcheck $? ${HAM_TOOLSET_NAME} "grpcurl version check failed." || return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

VER="--- grpcwebproxy -----------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  VER="$VER
Location: $(where_inpath grpcwebproxy)"
  errcheck $? ${HAM_TOOLSET_NAME}_grpcwebproxy "grpcwebproxy location check failed." || return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
