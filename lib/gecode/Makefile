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

EXTRA_LIBS =
EXTRA_INCLUDES = -I../..

MAKEDEPEND = alicetool makedepend

GECODEOBJ = \
	set/.libs/libgecodeset.a \
	int/.libs/libgecodeint.a \
	kernel/.libs/libgecodekernel.a

CXXFLAGS=-O2 -Dforceinline=inline -DNDEBUG -pipe
#CXXFLAGS=-Dforceinline=inline -pipe -g

all: $(NATIVES:%=%.dll) $(TARGETS)

UnsafeGecode.dll: $(SOURCES:%.cc=%.o) $(GECODEOBJ:%=../../%)

%.o: %.cc
	alicetool -v compile $(EXTRA_INCLUDES) $(CXXFLAGS) \
	-c  $< -o $@

%.dll: %.o
	alicetool -v link -g $(LDFLAGS) $(SOURCES:%.cc=%.o) $(GECODEOBJ:%=../../%) \
	$(EXTRA_LIBS) -o $@

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
	$(MAKEDEPEND) $(EXTRA_INCLUDES) $(SOURCES) >> Makefile.depend

install: all
	install -d $(INSTALLTO)
	install $(NATIVES:%=%.dll) $(TARGETS) $(INSTALLTO)

-include Makefile.depend
