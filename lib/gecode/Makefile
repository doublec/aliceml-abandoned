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


# Makefile for alice xml binding

INSTALLTO=`alice-config --alicelibdir`/gecode

NATIVES = UnsafeGecode

SOURCES = \
	UnsafeGecode.cc \
	GecodeSpace.cc

TARGETS = \
	SPACE-sig.stc \
	Space.stc \
	FD-sig.stc \
	FD.stc \
	FS-sig.stc \
	FS.stc \
	SEARCH-sig.stc \
	Search.stc \
	LINEAR-sig.stc \
	Linear.stc \
	CONSTRAINTS-sig.stc \
	Constraints.stc \
	DescParser.stc \
	Generator.stc \
	GeneratorMain.stc \

MAKEDEPEND = alicetool makedepend

GECODEOBJ = -lgecodeset -lgecodeint -lgecodekernel

CPPFLAGS = -I../../../gecode-install/include/gecode
LDFLAGS = -L../../../gecode-install/lib/gecode
#MY_CXXFLAGS=-O2 -DNDEBUG -pipe -DHAVE_CONFIG_H
MY_CXXFLAGS=-pipe -DHAVE_CONFIG_H

.PHONY: all generate clean veryclean distclean depend install

all: $(NATIVES:%=%.dll) $(TARGETS)

UnsafeGecode.dll: $(SOURCES:%.cc=%.o)

GeneratorMain.stc: Generator.stc

Generator.stc: DescParser.stc

UnsafeGecode.cc: UnsafeGecode.dsc GeneratorMain.stc
	alicerun GeneratorMain

UnsafeGecode.asig: UnsafeGecode.cc

%.o: %.cc
	alicetool -v compile $(EXTRA_INCLUDES) $(CXXFLAGS) $(MY_CXXFLAGS) \
	$(CPPFLAGS) -c  $< -o $@

%.dll: %.o
	alicetool -v link $(LDFLAGS) $(SOURCES:%.cc=%.o) \
	$(GECODEOBJ) $(EXTRA_LIBS) -o $@

%.stc: %.aml
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
	install -d $(INSTALLTO)
	install $(NATIVES:%=%.dll) $(TARGETS) $(INSTALLTO)

-include Makefile.depend
