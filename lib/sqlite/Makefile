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
INSTALLDIR = `alice-config --alicelibdir`/sqlite
SQLITEDIR = /usr

## No changes needed below this

NATIVES = UnsafeSQLite

SOURCES = \
	UnsafeSQLite.cc

TARGETS = SQLite.alc SQLITE-sig.alc

EXTRA_LIBS = -L$(SQLITEDIR)/lib -lsqlite3
EXTRA_INCLUDES = -I$(SQLITEDIR)/include

VERSION = 1.0.1
DISTFILES = $(SOURCES) Makefile debian
distdir = alice-sqlite-$(VERSION)

MAKEDEPEND = alicetool makedepend


all: compiledll compilealice
compiledll: $(NATIVES:%=%.dll)
compilealice: $(TARGETS)

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

install: installdll installalice

installdll: compiledll
	install -d $(INSTALLDIR)
	install $(NATIVES:%=%.dll) $(INSTALLDIR)

installalice: compilealice
	install -d $(INSTALLDIR)
	install $(TARGETS) $(INSTALLDIR)

### DISTRIBUTION OF THE C SOURCES FOR PACKAGING
#
distdir:
	rm -rf $(distdir)
	mkdir $(distdir)
	cp -r $(DISTFILES) $(distdir)

dist:	distdir
	tar chzf alice-sqlite-$(VERSION).tar.gz $(distdir)
	rm -rf $(distdir)

-include Makefile.depend
