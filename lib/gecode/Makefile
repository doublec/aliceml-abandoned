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
	Linear.stc

MAKEDEPEND = alicetool makedepend

GECODEOBJ = -lgecodeset -lgecodeint -lgecodekernel

MY_CXXFLAGS=-O2 -Dforceinline=inline -DNDEBUG -pipe -DHAVE_CONFIG_H

all: $(NATIVES:%=%.dll) $(TARGETS)

UnsafeGecode.dll: $(SOURCES:%.cc=%.o)

%.o: %.cc
	alicetool -v compile $(EXTRA_INCLUDES) $(CXXFLAGS) $(MY_CXXFLAGS) \
	-c  $< -o $@

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
	rm -f Makefile.depend

depend:
	alicedep *.aml > Makefile.depend
	$(MAKEDEPEND) $(EXTRA_INCLUDES) $(MY_CXXFLAGS) $(CXXFLAGS) $(SOURCES) \
	>> Makefile.depend

install: all
	install -d $(INSTALLTO)
	install $(NATIVES:%=%.dll) $(TARGETS) $(INSTALLTO)

-include Makefile.depend
