#!/bin/bash -e
echo "I/Setup ham environment"
. hat repos default

echo "I/Build ham"
(set -x;
 cd "$HAM_HOME/sources/ham" ;
 ./build-${HAM_BIN_LOA}.sh)

echo "I/Run ham tests"
(set -x;
 cd "$HAM_HOME/sources/ham/tests/ham_base" ;
 ham all)

echo "I/Build and run pi with ham"
(set -x;
 cd "$HAM_HOME/sources/ham/tests/pi" ;
 ham Run_pi)