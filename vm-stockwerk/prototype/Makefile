###
### Author:
###   Leif Kornstaedt <kornstae@ps.uni-sb.de>
###
### Copyright:
###   Leif Kornstaedt, 2002
###
### Last change:
###   $Date$ by $Author$
###   $Revision$
###

PLATFORM = $(shell oztool platform)

SRCS = \
	Debug.oz \
	Helper.oz \
	PrimitiveTable.oz \
	ByneedInterpreter.oz \
	Scheduler.oz \
	AbstractCodeInterpreter.oz \
	Pickle.oz \
	BootLinker.oz \
	Main.oz

LIB_SYSTEM_SRCS0 = \
	Config IO OS Unix CommandLine Component Debug Socket Rand Reflect
LIB_SYSTEM_SRCS = $(LIB_SYSTEM_SRCS0:%=lib/system/Unsafe%.oz)

LIB_UTILITY_SRCS0 = MkRefMap Addr
LIB_UTILITY_SRCS = $(LIB_UTILITY_SRCS0:%=lib/utility/Unsafe%.oz)

%.o: %.cc
	oztool c++ -c $< -o $@

%.so-$(PLATFORM): %.o
	oztool ld -o $@ $<

%.ozf: %.oz
	ozc -c $(OZCFLAGS) $< -o $@

all: \
	FloatChunk.so-$(PLATFORM) \
	$(SRCS:%.oz=%.ozf) \
	$(LIB_SYSTEM_SRCS:%.oz=%.ozf) \
	$(LIB_UTILITY_SRCS:%.oz=%.ozf)

stow.exe: $(SRCS:%.oz=%.ozf)
	ozl -v -o $@ Main.ozf

clean:
	-rm -f FloatChunk.o FloatChunk.so-$(PLATFORM)
	-rm -f $(SRCS:%.oz=%.ozf)
	-rm -f $(LIB_SYSTEM_SRCS:%.oz=%.ozf)
	-rm -f $(LIB_UTILITY_SRCS:%.oz=%.ozf)

PrimitiveTable.ozf: Helper.ozf
ByneedInterpreter.ozf: Helper.ozf
Scheduler.ozf: Helper.ozf
AbstractCodeInterpreter.ozf: Helper.ozf
Pickle.ozf: Helper.ozf
BootLinker.ozf: Helper.ozf
lib/system/UnsafeComponent.ozf: Helper.ozf
