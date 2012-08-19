# Makefile to build ham with Digital Mars C/C++ on Win32 systems
#
# To use it, you must be in the top Ham source directory,
# have the compiler in your path, and call:
#
#  set AGL_TOOLSET=DIGITALMARS
#  make -f builds\win32-dmars.mk
#
# the program "ham.exe" will be created in a new directory
# named "bin.ntx86"
#
CC        = dmc
CFLAGS    = -DNT 
TARGET    = -o ham0.exe
#LINKLIBS = oldnames.lib kernel32.lib libc.lib

all: ham0
	attrib -r jambase.c
	ham0

include common.mk
