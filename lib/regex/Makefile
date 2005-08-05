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

# Makefile for alice regex binding

INSTALLDIR = `alice-config --alicelibdir`/regex

ALICETOOL = alicetool

NATIVES = NativeRegex

SOURCES = \
	NativeRegex.cc regex.c

TARGETS = Regex.alc

EXTRA_LIBS = 
EXTRA_INCLUDES = -DSTDC_HEADERS

MAKEDEPEND = $(ALICETOOL) makedepend

VERSION = 1.2
DISTFILES = $(SOURCES) Makefile debian regex.h
distdir = alice-regex-$(VERSION)

all: compiledll compilealice
compiledll: $(NATIVES:%=%.dll)
compilealice: $(TARGETS)

%.o: %.cc
	$(ALICETOOL) compile $(EXTRA_INCLUDES) \
	-c  $< -o $@

%.o: %.c
	$(ALICETOOL) compilec $(EXTRA_INCLUDES) \
	-c  $< -o $@

%.dll: NativeRegex.o regex.o
	$(ALICETOOL) link $(EXTRA_LIBS) NativeRegex.o regex.o -o $@

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
	tar chzf alice-regex-$(VERSION).tar.gz $(distdir)
	rm -rf $(distdir)

-include Makefile.depend
