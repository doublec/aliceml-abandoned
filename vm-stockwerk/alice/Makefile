##
## Authors:
##   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Thorsten Brunklaus, 2002
##   Leif Kornstaedt, 2002-2003
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

LIGHTNING_SOURCES0 = \
	NativeConcreteCode.cc NativeCodeInterpreter.cc NativeCodeJitter.cc

ifdef LIGHTNING
LIGHTNING_SOURCES = $(LIGHTNING_SOURCES0)
else
LIGHTNING_SOURCES =
endif

PRIMITIVE_MODULES = \
	Unqualified Array Byte Char Future General GlobalStamp Hole Int \
	List Math Option Real Ref Remote String Thread UniqueString Unsafe \
	Vector Word8 Word8Array Word8Vector Word31
LIB_SYSTEM_MODULES = \
	Config IODesc OS CommandLine Component Debug Socket Rand Value \
	Reflect Foreign
LIB_UTILITY_MODULES = ImpMap Cell Addr
LIB_DISTRIBUTION_MODULES = Remote

SOURCES = \
	Data.cc Guid.cc AbstractCode.cc PrimitiveTable.cc \
	LazySelInterpreter.cc AbstractCodeInterpreter.cc AliceConcreteCode.cc \
	AliceLanguageLayer.cc BootLinker.cc \
	$(LIGHTNING_SOURCES) \
	$(PRIMITIVE_MODULES:%=primitives/%.cc) \
	$(LIB_SYSTEM_MODULES:%=lib/system/Unsafe%.cc) \
	$(LIB_UTILITY_MODULES:%=lib/utility/Unsafe%.cc) \
	$(LIB_DISTRIBUTION_MODULES:%=lib/distribution/Unsafe%.cc)

OBJS = $(SOURCES:%.cc=%.o)

all: $(OBJS)

clean:
	rm -f $(OBJS) $(LIGHTNING_SOURCES0:%.cc=%.o)

veryclean: clean

distclean: veryclean
	rm -f Makefile.depend

Makefile.depend: Makefile $(SOURCES)
	$(MAKEDEPEND) $(SOURCES) > $@

include Makefile.depend
