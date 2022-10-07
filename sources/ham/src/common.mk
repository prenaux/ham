# Common Makefile rules
#

# the Jam sources needed to build "ham0"
#
SOURCES = \
	buffer.c builtins.c command.c compile.c execunix.c expand.c \
	filent.c fileunix.c glob.c hash.c \
	hdrmacro.c headers.c jam.c jambase.c jamgram.c lists.c luagsub.c make.c make1.c \
	newstr.c option.c parse.c pathunix.c regexp.c \
	rules.c scan.c search.c sha256.c timestamp.c variable.c hcache.c

# the bootstrap "ham0" build tool
#
ham0:
	$(CC) $(TARGET) $(CFLAGS) $(SOURCES) $(LINKLIBS)
