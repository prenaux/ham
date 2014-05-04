#!/bin/bash

export ADR_CPU_TYPE=x86
toolset_import adr_42_base
if [ $? != 0 ]; then return 1; fi
