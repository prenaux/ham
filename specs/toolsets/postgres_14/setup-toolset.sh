#!/bin/bash

# for postgres_pass/postgres_connect/pgpass.py
toolset_import_once python_3 || return 1

# toolset
export HAM_TOOLSET=POSTGRES
export HAM_TOOLSET_NAME=postgres
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/postgres"

# path setup
case $HAM_OS in
  OSX*)
    ham-brew-install postgresql@14 "bin/postgres"
    POSTGRES_DIR="$(ham-brew-installdir postgresql@14)"
    export POSTGRES_DIR
    pathenv_add "${POSTGRES_DIR}/bin"
    ;;
  LINUX*)
    if [ ! -f "/usr/lib/postgresql/14/bin/psql" ]; then
      log_warning "psql not found, adding PostgreSQL APT repository and attempting to install postgresql-client-14..."
      (
        set -x
        sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
        wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
        sudo apt-get update -y
      )
      ham-apt-get-install postgresql-client-14
      if [ ! -f "/usr/lib/postgresql/14/bin/psql" ]; then
        log_error "ham-apt-get-install postgresql-client-14 install failed."
        return 1
      fi
    fi
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

# path
pathenv_add "${HAM_TOOLSET_DIR}"
export POSTGRES_DB_DIR="$WORK/Server/pg"

VER="--- postgres_14 -------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  if ! VER="$VER
$(psql --version)"; then
    echo "E/Can't get version."
    return 1
  fi
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
