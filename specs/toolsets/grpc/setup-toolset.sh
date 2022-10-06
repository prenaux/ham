#!/bin/bash

# toolset
export HAM_TOOLSET=GRPC
export HAM_TOOLSET_NAME=grpc
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/grpc"

# path setup
case $HAM_OS in
    OSX*)
        # Separated for clarity and easier error tracking

        # Protobuf, could move to it's own toolset.
        # XXX: protobuf@3 because protobuf install a buffy version of protobuf that breaks protoc-gen-js
        ham-brew-install protobuf@3 "bin/protoc" || return 1
        PREFIX=`ham-brew-installdir protobuf@3 prefix`
        export PROTOBUF_BINDIR="${PREFIX}/opt/protobuf"
        export PROTOBUF_INCDIR="${PREFIX}/include/google/protobuf"
        export PROTOBUF_LIBDIR="${PREFIX}/lib"

        # Grpc
        ham-brew-install grpc "bin/grpc_cli" || return 1
        PREFIX=`ham-brew-installdir grpc prefix`
        export GRPC_BINDIR="${PREFIX}/opt/grpc/bin"
        # ideally we'd use ${PREFIX}/include/grpc but there's also grpc++ &
        # grpcpp in there, its not clear which one we need and/or if we need
        # all of them
        export GRPC_INCDIR="${PREFIX}/include"
        export GRPC_LIBDIR="${PREFIX}/lib"

        # protoc-gen-grpc-web for client side Javascript
        ham-brew-install protoc-gen-grpc-web "bin/protoc-gen-grpc-web" || return 1
        export PROTOC_GEN_GRPC_WEB_EXE="`ham-brew-installdir protoc-gen-grpc-web bin`/bin/protoc-gen-grpc-web"

        # envoy, to proxy http2 -> http1 for client side Javascript. Might be
        # better as its own toolset but since the only know use case atm is
        # for grpc its simpler to have it here.
        ham-brew-install envoy "bin/envoy" || return 1
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

pathenv_add "$HAM_TOOLSET_DIR"

VER="--- protobuf --------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  ham-check-file "$PROTOBUF_INCDIR/message.h"
  errcheck $? ${HAM_TOOLSET_NAME}_protobuf "Include check failed." || return 1
  ham-check-file "$PROTOBUF_LIBDIR/libprotobuf.a"
  errcheck $? ${HAM_TOOLSET_NAME}_protobuf "Lib check failed." || return 1

  VER="$VER
`protoc --version`"
  errcheck $? ${HAM_TOOLSET_NAME}_protobuf "Version check failed." || return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

VER="--- protoc-gen-grpc-web ---------------"
if [ -z `where_inpath protoc-gen-grpc-web` ]; then
  die ${HAM_TOOLSET_NAME}_protoc-gen-grpc-web "Exe check failed." || return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

VER="--- grpc ------------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  ham-check-file "$GRPC_INCDIR/grpc/grpc.h"
  errcheck $? $HAM_TOOLSET_NAME "Include check failed." || return 1
  ham-check-file "$GRPC_LIBDIR/libgrpc.dylib"
  errcheck $? $HAM_TOOLSET_NAME "Lib check failed." || return 1

  # We check only for existence since grpc_cli doesnt have a version command :(
  ham-check-file "$GRPC_BINDIR/grpc_cli"
  errcheck $? $HAM_TOOLSET_NAME "Exe check failed." || return 1
  VER="$VER
grpc_cli"
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"

VER="--- envoy -----------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  VER="$VER
`envoy --version | grep version`"
  errcheck $? ${HAM_TOOLSET_NAME}_envoy "Version check failed." || return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
