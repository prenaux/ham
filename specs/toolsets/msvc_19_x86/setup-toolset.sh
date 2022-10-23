#!/bin/bash
export HAM_MSVC_ARCH=x86
toolset_import_force msvc_19_x64 || return 1
