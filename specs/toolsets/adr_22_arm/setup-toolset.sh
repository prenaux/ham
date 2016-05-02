#!/bin/bash

export ADR_CPU_TYPE=arm
export ADR_VERSION=22

toolset_import adr_base
if [ $? != 0 ]; then return 1; fi
