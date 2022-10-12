#!/bin/bash -e

# toolset
export HAM_TOOLSET=OPENSSL
export HAM_TOOLSET_NAME=openssl
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/openssl"

# path setup
case $HAM_OS in
    OSX*)
        # Separated for clarity and easier error tracking
        ham-brew-install openssl@3 "bin/openssl" || return 1
        PREFIX=`ham-brew-installdir openssl@3 prefix`
        export OPENSSL_BINDIR=`ham-brew-installdir openssl@3 bin`
        export OPENSSL_INCDIR=`ham-brew-installdir openssl@3 include`
        export OPENSSL_LIBDIR=`ham-brew-installdir openssl@3 lib`
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

pathenv_add "$HAM_TOOLSET_DIR"

VER="--- openssl ------------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
  ham-check-file "$OPENSSL_INCDIR/openssl/ssl.h"
  errcheck $? $HAM_TOOLSET_NAME "Include check failed." || return 1
  ham-check-file "$OPENSSL_LIBDIR/$(basename `ham-cppm-bin-filepath dll ssl`)"
  errcheck $? $HAM_TOOLSET_NAME "Lib check failed." || return 1

  # We check only for existence since openssl_cli doesnt have a version command :(
  ham-check-file "$OPENSSL_BINDIR/bin/openssl"
  errcheck $? $HAM_TOOLSET_NAME "Exe check failed." || return 1
  VER="$VER
`openssl version`"
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
