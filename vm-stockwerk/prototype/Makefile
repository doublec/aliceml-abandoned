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

SRCS = Primitives.oz Pickling.oz Emulator.oz

%.ozf: %.oz
	ozc -c $< -o $@

all: $(SRCS:%.oz=%.ozf)

clean:
	-rm -f $(SRCS:%.oz=%.ozf)
