#!/bin/bash
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

. ham-pm2 source

(set -ex ;
 ham-cppm-build thrift_cppm ;
 ./_build_thrift.sh ;
 ./_run_cpp_tests.sh)
