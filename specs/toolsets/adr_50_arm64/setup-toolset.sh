#!/bin/bash

export ADR_CPU_TYPE=arm64
export ADR_VERSION=50
export ADR_API=21

toolset_import adr_base
if [ $? != 0 ]; then return 1; fi
