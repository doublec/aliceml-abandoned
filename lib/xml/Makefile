### -*- Makefile -*-
###
### Author:
###   Guido Tack <tack@ps.uni-sb.de>
###
### Copyright:
###   Guido Tack, 2003
###
### See the file "LICENSE" for information on usage and
### redistribution of this file, and for a
###    DISCLAIMER OF ALL WARRANTIES.
###
### Last change:
###   $Date$ by $Author$
###   $Revision$
###

# Makefile for alice xml binding

INSTALLDIR = `alice-config --alicelibdir`/xml

NATIVES = NativeXML

SOURCES = \
	NativeXML.cc

TARGETS = XML.alc XML-sig.alc

VERSION = $(shell alicetool --package-version)
DISTFILES = $(SOURCES) Makefile debian
distdir = alice-xml-$(VERSION)

EXTRA_LIBS = `pkg-config --libs libxml-2.0`
EXTRA_INCLUDES = `pkg-config --cflags libxml-2.0`

all: compiledll compilealice
compiledll: $(NATIVES:%=%.dll)
compilealice: $(TARGETS)

XML.dll: $(SOURCES:%.cc=%.o)

%.o: %.cc
	alicetool compile $(EXTRA_INCLUDES) \
	-c  $< -o $@

%.dll: %.o
	alicetool link $(SOURCES:%.cc=%.o) $(EXTRA_LIBS) -o $@

%.alc: %.aml
	alicec -z $< -o $@

clean:
	rm -f $(NATIVES:%=%.o)
	rm -f $(TARGETS)

veryclean: clean
	rm -f $(NATIVES:%=%.dll.def)

distclean: veryclean
	rm -f $(NATIVES:%=%.dll)

depend:
	alicedep *.aml > Makefile.depend

install: all installdll installalice

installdll:
	install -d $(INSTALLDIR)
	install $(NATIVES:%=%.dll) $(INSTALLDIR)

installalice:
	install -d $(INSTALLDIR)
	install $(TARGETS) $(INSTALLDIR)

### DISTRIBUTION OF THE C SOURCES FOR PACKAGING
#
distdir:
	rm -rf $(distdir)
	mkdir $(distdir)
	cp -r $(DISTFILES) $(distdir)

dist:	distdir
	tar chzf alice-xml-$(VERSION).tar.gz $(distdir)
	rm -rf $(distdir)

-include Makefile.depend
