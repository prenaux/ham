# Makefile to build Ham with Cygwin on Win32 systems
#
# To use it, you must be in the top Ham source directory,
# have GCC compiler in your current path, and call:
#
#    make -f builds/win32-cygwin.mk
#
# the program "ham.exe" will be created in the new
# directory named "bin.cygwinx86"
#
# note that the resulting executable will only be usable
# under Cygwin, since it will output Unix commands. I.e.
# it will _not_ use AGL_TOOLSET and won't be able to compile
# with Mingw, Visual C++, Borland C++ and other native
# Win32 compilers
#

CC     = gcc
TARGET = -o ham0.exe
CFLAGS = -D__cygwin__

all: ham0
	attrib -r jambase.c
	ham0

include common.mk
