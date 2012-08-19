# Makefile to build Ham with Borland C on Win32 systems
#
# To use it, you must be in the top Ham source directory,
# have the Borland C++ compiler in your current path, and
# call:
#
#    make -fbuilds\win32-borlandc.mk
#
# the program "ham.exe" will be created in the new
# directory named "bin.ntx86"
#

CC     = bcc32
TARGET = -eham0
CFLAGS = /DNT -w- -q

all: ham0
	attrib -r jambase.c
	ham0

!include common.mk
