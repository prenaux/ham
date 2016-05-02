#!/bin/bash

export HAM_MSVC_ARCH=x86

toolset_import msvc_15_x64
if [ $? != 0 ]; then return 1; fi
