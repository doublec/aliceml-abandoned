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
	PrimitiveTable.oz \
	ByneedInterpreter.oz \
	Scheduler.oz \
	AbstractCodeInterpreter.oz \
	Pickle.oz \
	Linker.oz \
	Main.oz

LIB_SYSTEM_SRCS = UnsafeOS.oz

%.o: %.cc
	oztool c++ -c $< -o $@

%.so-$(PLATFORM): %.o
	oztool ld -o $@ $<

%.ozf: %.oz
	ozc -c $< -o $@

all: \
	FloatChunk.so-$(PLATFORM) \
	$(SRCS:%.oz=%.ozf) \
	$(LIB_SYSTEM_SRCS:%.oz=lib/system/%.ozf)

clean:
	-rm -f FloatChunk.o FloatChunk.so-$(PLATFORM)
	-rm -f $(SRCS:%.oz=%.ozf)
	-rm -f $(LIB_SYSTEM_SRCS:%.oz=lib/system/%.ozf)
