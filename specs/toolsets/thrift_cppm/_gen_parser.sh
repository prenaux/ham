#!/bin/bash -ex
mkdir -p ./github-thrift/compiler/cpp/src/thrift/gen/

ham-brew-install bison "bin/bison"
"$(ham-brew-installdir bison)/bin/bison" --output=./github-thrift/compiler/cpp/src/thrift/thrifty.cc --header=./github-thrift/compiler/cpp/src/thrift/thrifty.hh ./github-thrift/compiler/cpp/src/thrift/thrifty.yy

ham-brew-install flex "bin/flex"
"$(ham-brew-installdir flex)/bin/flex" -o ./github-thrift/compiler/cpp/src/thrift/thriftl.cc ./github-thrift/compiler/cpp/src/thrift/thriftl.ll
