#!/bin/bash
export HAM_NO_VER_CHECK=1
. hat > /dev/null
set -ex
simple-http-server regular
