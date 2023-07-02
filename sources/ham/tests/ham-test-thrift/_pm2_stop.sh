#!/bin/bash
. ham-pm2 source
set -ex

# Stop all processes
ham-pm2 stop all
