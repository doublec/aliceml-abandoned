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
	Pickle.oz \
	AbstractCodeInterpreter.oz \
	Scheduler.oz \
	Linker.oz \
	Main.oz

%.o: %.cc
	oztool c++ -c $< -o $@

%.so-$(PLATFORM): %.o
	oztool ld -o $@ $<

%.ozf: %.oz
	ozc -c $< -o $@

all: FloatChunk.so-$(PLATFORM) $(SRCS:%.oz=%.ozf)

clean:
	-rm -f $(SRCS:%.oz=%.ozf)
