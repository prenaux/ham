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

echo "I/Build cppm modules."
case $HAM_OS in
    NT*)
        echo "W/thrift_cppm module not supported yet on Windows."
        ;;
    OSX*)
        (set -x ;
         ham-cppm-build thrift_cppm)
        ;;
    LINUX*)
        (set -x ;
         ham-cppm-build thrift_cppm)
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
