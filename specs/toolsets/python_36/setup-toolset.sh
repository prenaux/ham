#!/bin/bash
echo "W/Deprecated toolset 'python_36', replace with 'python_3'."
toolset_import python_3 || return 1
