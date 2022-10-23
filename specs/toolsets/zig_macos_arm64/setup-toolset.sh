#!/bin/bash
export HAM_TARGET_BIN_LOA=osx-arm64
toolset_import_strict zig || return 1
