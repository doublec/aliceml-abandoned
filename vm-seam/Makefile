##
## Author:
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Leif Kornstaedt, 2000-2003
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

include Makefile.vars
include Makefile.rules

PRIMITIVE_MODULES = \
	Unqualified Array Byte Char Future General GlobalStamp Hole Int \
	List Math Option Real Ref Remote String Thread UniqueString Unsafe \
	Vector Word8 Word8Array Word8Vector Word31
PRIMITIVE_SOURCES = $(PRIMITIVE_MODULES:%=alice/primitives/%.cc)
LIB_SYSTEM_MODULES = \
	Config IODesc OS CommandLine Component Debug Socket Rand Value \
	Reflect Foreign
LIB_SYSTEM_SOURCES = $(LIB_SYSTEM_MODULES:%=alice/lib/system/Unsafe%.cc)
LIB_UTILITY_MODULES = ImpMap Cell Addr
LIB_UTILITY_SOURCES = $(LIB_UTILITY_MODULES:%=alice/lib/utility/Unsafe%.cc)
LIB_DISTRIBUTION_MODULES = Remote
LIB_DISTRIBUTION_SOURCES = \
	$(LIB_DISTRIBUTION_MODULES:%=alice/lib/distribution/Unsafe%.cc)
ALICE_SOURCES0 = \
	Data.cc Guid.cc AbstractCode.cc PrimitiveTable.cc \
	LazySelInterpreter.cc AbstractCodeInterpreter.cc AliceConcreteCode.cc \
	AliceLanguageLayer.cc BootLinker.cc \
	NativeConcreteCode.cc NativeCodeInterpreter.cc NativeCodeJitter.cc
ALICE_SOURCES = $(ALICE_SOURCES0:%=alice/%)
BASE_SOURCES = AliceMain.cc
SOURCES = \
	$(BASE_SOURCES) \
	$(ALICE_SOURCES) \
	$(PRIMITIVE_SOURCES) \
	$(LIB_SYSTEM_SOURCES) \
	$(LIB_UTILITY_SOURCES) \
	$(LIB_DISTRIBUTION_SOURCES)
OBJS = $(SOURCES:%.cc=%.o)

all: alice.dll

alice.dll: $(OBJS)
	$(LD) $(LDFLAGS) -shared -o $@ $(OBJS) $(LIBS)

clean:
	rm -f $(OBJS)
veryclean: clean
	rm -f alice.dll
distclean: veryclean
	rm -f Makefile.depend

Makefile.depend: Makefile $(SOURCES)
	($(MAKEDEPEND) $(BASE_SOURCES) && \
	 for i in $(ALICE_SOURCES); do \
	     $(MAKEDEPEND) $$i -MT alice/`basename $$i .cc`.o; \
	 done && \
	 for i in $(PRIMITIVE_SOURCES); do \
	     $(MAKEDEPEND) $$i -MT alice/primitives/`basename $$i .cc`.o; \
	 done && \
	 for i in $(LIB_SYSTEM_SOURCES); do \
	     $(MAKEDEPEND) $$i -MT alice/lib/system/`basename $$i .cc`.o; \
	 done && \
	 for i in $(LIB_UTILITY_SOURCES); do \
	     $(MAKEDEPEND) $$i -MT alice/lib/utility/`basename $$i .cc`.o; \
	 done && \
	 for i in $(LIB_DISTRIBUTION_SOURCES); do \
	     $(MAKEDEPEND) $$i -MT alice/lib/distribution/`basename $$i .cc`.o; \
	 done) > $@

include Makefile.depend
