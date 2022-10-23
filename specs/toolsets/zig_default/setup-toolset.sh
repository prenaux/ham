#!/bin/bash
export HAM_TARGET_BIN_LOA=$HAM_BIN_LOA
toolset_import_strict zig || return 1
