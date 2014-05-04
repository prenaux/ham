#!/bin/bash

# toolset
export HAM_TOOLSET=PERL
export HAM_TOOLSET_VER=516
export HAM_TOOLSET_NAME=perl
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/perl"

# path setup
case $HAM_OS in
    NT*)
        export STRAW_PERL_HOME="${HAM_TOOLSET_DIR}/nt-x86/"
        export PATH=${STRAW_PERL_HOME}:${PATH}
        if [ ! -e "$STRAW_PERL_HOME" ]; then
            toolset_dl perl perl_nt-x86
            if [ ! -e "$STRAW_PERL_HOME" ]; then
                echo "E/nt-x86 folder doesn't exist in the toolset"
                return 1
            fi
        fi
        export PATH=$STRAW_PERL_HOME/perl/site/bin:$STRAW_PERL_HOME/perl/bin:$STRAW_PERL_HOME/c/bin:$PATH
        ;;
    OSX*)
        # OSX has Perl v5.12.4 pre-installed, which seems to do the job for our needs...
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

export PERL_JSON_BACKEND=
export PERL_YAML_BACKEND=
export PERL5LIB=
export PERL5OPT=
export PERL_MM_OPT=
export PERL_MB_OPT=

VER="--- perl ------------------------"
if [ "$HAM_NO_VER_CHECK" != "1" ]; then
    VER="$VER
`perl --version | grep 'perl' | head -n 1`"
    if [ $? != 0 ]; then
        echo "E/Can't get version."
        return 1
    fi
fi

export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
