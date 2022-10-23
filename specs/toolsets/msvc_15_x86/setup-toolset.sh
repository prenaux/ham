#!/bin/bash
export HAM_MSVC_ARCH=x86
toolset_import_strict msvc_15_x64 || return 1
