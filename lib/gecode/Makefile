###
### Authors:
###  Guido Tack <tack@ps.uni-sb.de>
###
### Copyright:
###  Guido Tack, 2003
###
###  Last change:
###    $Date$ by $Author$
###    $Revision$
### 


# Makefile for alice gecode binding

VERSION=1.2

INSTALLDIR=`alice-config --alicelibdir`/gecode
GECODEDIR="/usr"

NATIVES = UnsafeGecode

SOURCES = \
	UnsafeGecode.cc \
	GecodeSpace.cc

TARGETS = \
	SPACE-sig.alc \
	Space.alc \
	FD-sig.alc \
	FD.alc \
	FS-sig.alc \
	FS.alc \
	SEARCH-sig.alc \
	Search.alc \
	LINEAR-sig.alc \
	Linear.alc \
	CONSTRAINTS-sig.alc \
	Constraints.alc

DISTFILES = \
	debian \
	Makefile \
	GecodeSpace.cc \
	GecodeSpace.hh \
	UnsafeGecode.cc \
	UnsafeGecodeSpecial.hh

distdir = alice-gecode-$(VERSION)

MAKEDEPEND = alicetool makedepend

CPPFLAGS = `pkg-config --cflags gecode`
LDFLAGS = `pkg-config --libs gecode`

.PHONY: all generate clean veryclean distclean depend install dist

all: compiledll compilealice

compiledll: $(NATIVES:%=%.dll)
compilealice: $(TARGETS)

UnsafeGecode.dll: $(SOURCES:%.cc=%.o)

GeneratorMain.alc: Generator.alc

Generator.alc: DescParser.alc

%.o: %.cc
	alicetool -v compile $(CXXFLAGS) \
	$(CPPFLAGS) -c  $< -o $@


%.dll: %.o
	alicetool -v link $(LDFLAGS) $(SOURCES:%.cc=%.o) -o $@

%.alc: %.aml
	alicec -z $< -o $@

clean:
	rm -f $(NATIVES:%=%.o) $(SOURCES:%.cc=%.o)
	rm -f $(TARGETS)

veryclean: clean
	rm -f $(NATIVES:%=%.dll.def)

distclean: veryclean
	rm -f $(NATIVES:%=%.dll)
	rm -f UnsafeGecode.cc UnsafeGecode.asig
	rm -f Makefile.depend

depend: UnsafeGecode.cc
	alicedep *.aml > Makefile.depend
	$(MAKEDEPEND) $(EXTRA_INCLUDES) $(CXXFLAGS) $(SOURCES) \
	$(CPPFLAGS) >> Makefile.depend

install: installdll installalice

installdll: compiledll
	mkdir -p -m 775 $(INSTALLDIR)
	install -c -m555 $(NATIVES:%=%.dll) $(INSTALLDIR)

installalice: compilealice
	mkdir -p -m 775 $(INSTALLDIR)
	install -c -m444 $(TARGETS) $(INSTALLDIR)

generate: UnsafeGecode.dsc GeneratorMain.alc
	alicerun GeneratorMain

distdir:
	rm -rf $(distdir)
	mkdir $(distdir)
	cp -r $(DISTFILES) $(distdir)
dist: distdir
	tar chzf alice-gecode-$(VERSION).tar.gz $(distdir)
	rm -r $(distdir)

-include Makefile.depend
