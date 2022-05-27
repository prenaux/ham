#!/bin/bash
export HAM_TARGET_BIN_LOA=$HAM_BIN_LOA
toolset_import zig
if [ $? != 0 ]; then return 1; fi
