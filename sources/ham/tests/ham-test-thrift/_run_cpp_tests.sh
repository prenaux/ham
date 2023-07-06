#!/bin/bash
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

. ham-pm2 source

function wait_for_thrift_server() {
    (set +ex ;
     echo -n "I/Waiting for thrift server '$1' to start..."
     for try in {1..100} ; do
         RES=`curl --silent --head --fail "$1" > /dev/null ; echo $?`
         # echo "... RES: $RES"
         echo -n "."
         # 0 is a regular a regular response
         # 52 is an empty server response which is what curl returns for a started http thrift server
         if [ "$RES" == "0" ] || [ "$RES" == "52" ]; then
             echo ""
             return 0
         fi
         sleep .1
     done;
     echo ""
     echo "E/Server took too long to start."
     return 1)
}

set -ex

# Build client & server.
# Note: Do this first so any error message are printed.
HAM_NO_VER_CHECK=1 ham-cppm-build thrift_cppm
ham ham-test-thrift-server ham-test-thrift-client

# Start the C++ Server
ham-pm2 restart ham-test-thrift-cpp-server ./_serve_cpp_server.sh
function stop-ham-test-thrift-cpp-server {
    TRAPPED_ERROR_CODE=$?
    # Stop the C++ server
    ham-pm2 stop ham-test-thrift-cpp-server
    exit $TRAPPED_ERROR_CODE
}
trap stop-ham-test-thrift-cpp-server EXIT

# Give it a sec to start...
# sleep .5
wait_for_thrift_server http://localhost:40990

# Run the client, our test...
ham Run_ham-test-thrift-client
