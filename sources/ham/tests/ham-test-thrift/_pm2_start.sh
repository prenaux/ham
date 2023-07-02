#!/bin/bash
. ham-pm2 source
set -ex

# Sanity check to make sure pm2 actually works and prints the version number
ham-pm2 -v

# Start the static file hosting to test the HTML/JS samples
ham-pm2 start-once ham-test-thrift-static-files ./_serve_static_files.sh

# Build & start the C++ Server
ham-pm2 start-once ham-test-thrift-cpp-server ./_serve_cpp_server.sh
