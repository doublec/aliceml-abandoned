### -*- Makefile -*-
###
### Author:
###   Guido Tack <tack@ps.uni-sb.de>
###
### Copyright:
###   Guido Tack, 2003
###
### Last change:
###   $Date$ by $Author$
###   $Revision$
###

# Makefile for Alice/SEAM sqlite binding

# Change this to reflect your installation directory
INSTALLTO = `alice-config --alicelibdir`/sqlite
SQLITEDIR = /usr

## No changes needed below this

NATIVES = UnsafeSQLite

SOURCES = \
	UnsafeSQLite.cc

TARGETS = SQLite.alc SQLITE-sig.alc

EXTRA_LIBS = -L$(SQLITEDIR)/lib -lsqlite3
EXTRA_INCLUDES = -I$(SQLITEDIR)/include

MAKEDEPEND = alicetool makedepend


all: $(NATIVES:%=%.dll) $(TARGETS)

%.o: %.cc
	alicetool -v compile $(EXTRA_INCLUDES) \
	-c  $< -o $@

%.dll: %.o
	alicetool -v link $(LDFLAGS) $(SOURCES:%.cc=%.o) $(EXTRA_LIBS) -o $@

%.alc: %.aml
	alicec -c $< -o $@

clean:
	rm -f $(NATIVES:%=%.o)
	rm -f $(TARGETS)

veryclean: clean
	rm -f $(NATIVES:%=%.dll.def)

distclean: veryclean
	rm -f $(NATIVES:%=%.dll)
	rm -f Makefile.depend

depend:
	alicedep *.aml > Makefile.depend
#	$(MAKEDEPEND) $(SOURCES) >> Makefile.depend

install: all
	install -d $(INSTALLTO)
	install $(NATIVES:%=%.dll) $(NATIVES:%=%.asig) $(TARGETS) $(INSTALLTO)

-include Makefile.depend
