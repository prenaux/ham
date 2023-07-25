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
 ham -X ham/sources/ham/tests/pi Run_pi)

echo "I/Test examples"
(set -x;
 ham -X ham/sources/examples/py3-venv-web-scrape _run_ci.sh)

echo "I/Build cppm modules."
case $HAM_OS in
    NT*|OSX*|LINUX*)
        (set -x ;
         "$HAM_HOME/sources/ham/tests/ham-test-thrift/_run_ci.sh")
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac
