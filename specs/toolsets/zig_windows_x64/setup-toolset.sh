#!/bin/bash
export HAM_TARGET_BIN_LOA=nt-x86
toolset_import zig
if [ $? != 0 ]; then return 1; fi
