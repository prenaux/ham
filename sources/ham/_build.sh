#!/bin/bash
. ham-bash-lib.sh

HAM_NO_VER_CHECK=1 . hat
errcheck $? _build_ham "Can't setup toolset"

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SDIR="${SCRIPT_DIR}/src"

build_jambase() {
  ODIR="bin/${HAM_BIN_LOA}"

  echo "I/build_jambase: Create output folder "$ODIR""
  (set -x ; mkdir -p "$ODIR")

  echo "I/build_jambase: Building jambase.c..."
  (set -x ;
   zig cc -o "${ODIR}/mkjambase" mkjambase.c ;
   "$ODIR/mkjambase" jambase.c Jambase)
}

build_ham() {
  BIN_LOA=$1
  EXE_NAME=$2
  ODIR="bin/$BIN_LOA"
  ZIG_TARGET=$(zig-get-bin-loa-target ${BIN_LOA})

  echo "I/build_ham: $BIN_LOA ($ZIG_TARGET): Create output folder "$ODIR""
  (set -x ; mkdir -p "$ODIR")

  echo "I/build_ham: $BIN_LOA ($ZIG_TARGET): Building ${EXE_NAME}"
  (set -x ;
   zig cc -o "${ODIR}/${EXE_NAME}" -g0 -O2 \
       -target ${ZIG_TARGET} \
       -Wno-incompatible-pointer-types-discards-qualifiers \
       -Wno-parentheses -Wno-string-plus-int \
       buffer.c builtins.c command.c compile.c \
       execunix.c expand.c \
       filent.c fileunix.c glob.c hash.c \
       hdrmacro.c headers.c \
       jam.c jambase.c jamgram.c \
       lists.c luagsub.c make.c make1.c \
       newstr.c option.c parse.c pathunix.c regexp.c \
       rules.c scan.c search.c sha256.c timestamp.c variable.c hcache.c )
}

unquarantine_output() {
  echo "I/unquarantine_output: $1"
  case "$HAM_BIN_LOA" in
    osx-*)
      (set -x ; sudo xattr -r -d com.apple.quarantine "$1")
    ;;
  esac
}

build_ham_cp() {
  BIN_LOA=$1
  EXE_NAME=$2
  ODIR="bin/$BIN_LOA"

  echo "I/build_ham_cp: $BIN_LOA ($ZIG_TARGET): Copying to HAM_HOME/bin"
  (set -x ;
   mkdir -p "$HAM_HOME/bin/${BIN_LOA}/" ;
   # Delete the destination file explicitly so that macOS doesn't end up hard
   # killing it.
   rm -f "$HAM_HOME/bin/${BIN_LOA}/${EXE_NAME}" ;
   cp "${ODIR}/${EXE_NAME}" "$HAM_HOME/bin/${BIN_LOA}/${EXE_NAME}")

  unquarantine_output "$HAM_HOME/bin/${BIN_LOA}/${EXE_NAME}"
}

build_ham0() {
  ODIR="bin/${HAM_BIN_LOA}"

  build_ham ${HAM_BIN_LOA} ham0

  echo "I/Run ham0"
  (set -x ;
   "${ODIR}/ham0")

  case "$HAM_BIN_LOA" in
    nt-*) EXE_NAME=ham.exe ;;
    *) EXE_NAME=ham ;;
  esac
  build_ham_cp ${HAM_BIN_LOA} ${EXE_NAME}
}

build_ham_crosscompile() {
  BIN_LOA=$1
  EXE_NAME=$2
  build_ham ${BIN_LOA} ${EXE_NAME}
  build_ham_cp ${BIN_LOA} ${EXE_NAME}
}

cd "$SDIR"
build_jambase
build_ham0
build_ham_crosscompile osx-x64 ham
build_ham_crosscompile osx-arm64 ham
build_ham_crosscompile lin-x64 ham
build_ham_crosscompile nt-x86 ham.exe

echo "I/Removing intermediate files."
rm -Rf "$SDIR/bin/"

# XXX: Add a proper "clean" target that delete zig's internal cache which
# prevents it from recompiling when it should.
echo "W/!!! zig cc's cache is not accurate and doesn't always rebuild when it should !!!"
echo "I/Done."
