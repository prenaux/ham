#!/bin/bash
export ADR_CPU_TYPE=arm
export ADR_VERSION=50
export ADR_API=21
toolset_import_strict adr_base || return 1
