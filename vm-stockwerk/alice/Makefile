##
## Authors:
##   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Thorsten Brunklaus, 2002
##   Leif Kornstaedt, 2002
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

PRIMITIVE_MODULES = \
	Unqualified Array Char Future General GlobalStamp Hole Int LargeWord \
	List Math Option Real Ref String Thread Unsafe Vector Word
LIB_SYSTEM_MODULES = \
	Config IO OS CommandLine Component Debug Rand Reflect Socket
LIB_UTILITY_MODULES = MkRefMap
LIB_DISTRIBUTION_MODULES = Remote

SOURCES = \
	Outline.cc Data.cc Guid.cc PrimitiveTable.cc \
	$(PRIMITIVE_MODULES:%=primitives/%.cc) \
	$(LIB_SYSTEM_MODULES:%=lib/system/Unsafe%.cc) \
	$(LIB_UTILITY_MODULES:%=lib/utility/Unsafe%.cc) \
	$(LIB_DISTRIBUTION_MODULES:%=lib/distribution/Unsafe%.cc) \
	LazySelInterpreter.cc AbstractCodeInterpreter.cc AliceConcreteCode.cc \
	AliceLanguageLayer.cc

OBJS = $(SOURCES:%.cc=%.o)

all: libalice.a

libalice.a: $(OBJS)
	ar rc libalice.a $(OBJS) || exit 1
	ranlib libalice.a

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libalice.a

distclean: veryclean

Makefile.depend: Makefile $(SOURCES)
	$(MAKEDEPEND) $(SOURCES) > Makefile.depend

include Makefile.depend
