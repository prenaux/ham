# Makefile to build jam with Visual C on Win32 systems
#
# To use it, you must be in the top Jam source directory,
# have the compiler in your path, and call:
#
#  nmake -f builds\win32-visualc.mk
#
# the program "jam.exe" will be created in a new directory
# named "bin.ntx86"
#
CC       = cl /nologo
CFLAGS   = -DNT -O2 -GL # Release
# CFLAGS   = -DNT -Od -Zi -RTC1 # Debug...
TARGET   = /Feham0
LINKLIBS = oldnames.lib kernel32.lib

all: ham0
	ham0

!include common.mk
