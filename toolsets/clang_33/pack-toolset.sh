#!/bin/bash
export HAM_NO_VER_CHECK=1
if [ -z "$HAM_HOME" ]; then
    HAM_HOME=`pwd`/"../../"
fi
. "$HAM_HOME/bin/ham-bash-setenv.sh"
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

7z-pack clang_33_nt-x86 nt-x86/
