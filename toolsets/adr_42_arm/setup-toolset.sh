#!/bin/bash

export ADR_CPU_TYPE=arm
toolset_import adr_42_base
if [ $? != 0 ]; then return 1; fi
