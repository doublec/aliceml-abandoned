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

INSTALLTO=`alice-config --alicelibdir`/gecode
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
	Constraints.alc \
	DescParser.alc \
	Generator.alc \
	GeneratorMain.alc \

MAKEDEPEND = alicetool makedepend

GECODEOBJ = -lgecodeset -lgecodeint -lgecodekernel

CPPFLAGS = -I$(GECODEDIR)/include/gecode
LDFLAGS = -L$(GECODEDIR)/lib/gecode

MY_CXXFLAGS=-O3 -finline-limit=3000 -DNDEBUG -pipe -DHAVE_CONFIG_H
#MY_CXXFLAGS=-ggdb -pipe -DHAVE_CONFIG_H

.PHONY: all generate clean veryclean distclean depend install

all: $(NATIVES:%=%.dll) $(TARGETS)

UnsafeGecode.dll: $(SOURCES:%.cc=%.o)

GeneratorMain.alc: Generator.alc

Generator.alc: DescParser.alc

UnsafeGecode.cc: UnsafeGecode.dsc GeneratorMain.alc
	alicerun GeneratorMain

UnsafeGecode.asig: UnsafeGecode.cc

%.o: %.cc
	alicetool -v compile $(EXTRA_INCLUDES) $(CXXFLAGS) $(MY_CXXFLAGS) \
	$(CPPFLAGS) -c  $< -o $@

%.dll: %.o
	alicetool -v link $(LDFLAGS) $(SOURCES:%.cc=%.o) \
	$(GECODEOBJ) $(EXTRA_LIBS) -o $@

%.alc: %.aml
	alicec -c $< -o $@

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
	$(MAKEDEPEND) $(EXTRA_INCLUDES) $(MY_CXXFLAGS) $(CXXFLAGS) $(SOURCES) \
	$(CPPFLAGS) >> Makefile.depend

install: all
	mkdir -p -m 775 $(INSTALLTO)
	install -c -m555 $(NATIVES:%=%.dll)
	install -c -m444 $(TARGETS) $(INSTALLTO)

-include Makefile.depend
