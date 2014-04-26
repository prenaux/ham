#!/bin/bash

# toolset
export HAM_TOOLSET=TEX
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=tex
export HAM_TOOLSET_DIR=${HAM_HOME}/toolsets/tex

# path setup
case $HAM_OS in
    NT*)
        export TEXLIVE=$HAM_TOOLSET_DIR/nt-x86
        if [ ! -e $TEXLIVE ]; then
            toolset_dl tex tex_nt-x86
            if [ ! -e $TEXLIVE ]; then
                echo "E/tex nt-x86 folder doesn't exist in the toolset and can't be downloaded"
                return 1
            fi
        fi
        export TEXOS="win32"
        export TEXROOT=$TEXLIVE
        export TLROOT=$TEXROOT
        export TEXMFTEMP=$TEXROOT/temp
        export TEXMFVAR=$TEXROOT/texmf-var
        export TEXMFCNF=$TEXROOT/texmf-dist/web2c
        export TEXISO_BIN=$TEXLIVE/bin/win32
        export PATH=$PATH:$TEXISO_BIN
        if [ ! -e "$TEXROOT" ]; then
            echo "E/Can't find TEXROOT directory: $TEXROOT"
            return 1
        fi
        if [ ! -e "$TEXLIVE/bin/win32" ]; then
            echo "E/TeXLive ISO not mounted in $TEXLIVE ($TEXROOT)"
            return 1
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

VER="--- tex ------------------------
`$TEXISO_BIN/pdflatex -v | head -n 1`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
